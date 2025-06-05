package br.edu.ifba.aem.ui.pages.types;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.EventField;
import br.edu.ifba.aem.ui.components.FieldType;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.components.PersonField;
import br.edu.ifba.aem.ui.pages.Page;
import br.edu.ifba.aem.ui.providers.JLineInteractionProvider;
import br.edu.ifba.aem.ui.providers.StdoutInteractionProvider;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import org.jline.consoleui.elements.ConfirmChoice;
import org.jline.consoleui.elements.PromptableElementIF;
import org.jline.consoleui.prompt.ConfirmResult;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.consoleui.prompt.InputResult;
import org.jline.consoleui.prompt.ListResult;
import org.jline.consoleui.prompt.PromptResultItemIF;
import org.jline.consoleui.prompt.builder.PromptBuilder;
import org.jline.reader.LineReader;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStyle;

public abstract class FormPage extends Page {

  public FormPage(String title) {
    super(title);
  }

  protected abstract List<FormField<?>> getFormFields();

  public abstract void onInterruptScreen(InteractionProvider provider);

  protected abstract void onSubmit(InteractionProvider provider, Map<String, Object> results);

  protected List<AttributedString> getFormHeader() {
    return new ArrayList<>();
  }

  @Override
  public void display(InteractionProvider provider) {
    provider.clearScreen();
    renderTitle(provider);
    renderComponents(provider);

    List<FormField<?>> initialFields = getFormFields();

    if (initialFields == null || initialFields.isEmpty()) {
      provider.getWriter().println("No form fields defined for this page.");
      onSubmit(provider, Collections.emptyMap());
      return;
    }

    try {
      Map<String, ?> rawResults = gatherRawInputs(provider, initialFields);

      if (rawResults == null) {
        onSubmit(provider, Collections.emptyMap());
        return;
      }

      boolean isJLine = isJLineProvider(provider);
      List<FormField<?>> allPromptedFields = getFormFieldsRecursively(initialFields, rawResults,
          isJLine);
      Map<String, Object> parsedResults = parseResults(provider, rawResults, allPromptedFields);

      onSubmit(provider, parsedResults);
    } catch (UserInterruptException exception) {
      provider.getWriter().println("\nForm cancelled by user.");
      onInterruptScreen(provider);
    } catch (Exception exception) {
      provider.getWriter()
          .println("An error occurred during form processing: " + exception.getMessage());

      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace(provider.getWriter());
      }

      onSubmit(provider, Collections.emptyMap());
    }
  }

  private boolean isJLineProvider(InteractionProvider provider) {
    return provider.getNativeProvider() instanceof JLineInteractionProvider;
  }

  private Map<String, ?> gatherRawInputs(InteractionProvider provider,
      List<FormField<?>> initialFields)
      throws IOException, UserInterruptException {
    Object nativeProvider = provider.getNativeProvider();
    if (nativeProvider instanceof JLineInteractionProvider jlip) {
      return gatherJLineInput(jlip, initialFields, this::getFormHeader);
    }
    if (nativeProvider instanceof StdoutInteractionProvider) {
      return gatherStdoutInput(provider, initialFields);
    }
    provider.getWriter().println("Unsupported interaction provider for forms.");
    return null;
  }

  private Map<String, Object> parseResults(InteractionProvider provider,
      Map<String, ?> rawResults,
      List<FormField<?>> allPromptedFields) {
    Map<String, Object> parsedResults = new HashMap<>();
    boolean isJLine = isJLineProvider(provider);

    for (FormField<?> field : allPromptedFields) {
      Object rawValue = rawResults.get(field.getName());
      if (rawValue == null && !(isJLine && field.getFieldType() == FieldType.BOOLEAN)) {
        continue;
      }

      String stringValue;
      if (isJLine) {
        // Ensure rawValue is PromptResultItemIF for JLine path
        if (!(rawValue instanceof PromptResultItemIF)) {
          // This case should ideally not happen if gatherRawInputs for JLine returns Map<String, PromptResultItemIF>
          if (AppConfig.DEBUG_MODE) {
            provider.getWriter().println("Warning: JLine raw result for field '" + field.getName() + "' is not a PromptResultItemIF: " + (rawValue != null ? rawValue.getClass().getName() : "null"));
          }
          continue;
        }
        PromptResultItemIF item = (PromptResultItemIF) rawValue;
        if (field.getFieldType() == FieldType.BOOLEAN) {
          if (item instanceof ConfirmResult cr) {
            boolean confirmed = cr.getConfirmed() == ConfirmChoice.ConfirmationValue.YES;
            parsedResults.put(field.getName(), confirmed);
          }
          continue; // Already processed or skip if not ConfirmResult
        }
        stringValue = extractStringFromJLineResult(item);
      } else {
        stringValue = (String) rawValue;
      }

      if (stringValue == null && field.getFieldType() != FieldType.BOOLEAN) { // Keep allowing null for non-boolean if parser handles it
        // However, if the field was prompted and returned null, it might mean empty input
        // which could be invalid or a deliberate null. Parser should decide.
        // If stringValue is null from extractStringFromJLineResult, it means the result was null.
      }


      Function<String, ?> parser = field.getParser();

      if (parser != null) {
        try {
          // For fields that might have null as a valid parsed value from an empty string,
          // or if stringValue itself is null.
          parsedResults.put(field.getName(), parser.apply(stringValue));
        } catch (Exception parseException) {
          String errorMsg = String.format("Error parsing value for field '%s': value was \"%s\". Error: %s",
              field.getLabel(), stringValue, parseException.getMessage());
          provider.getWriter().println(errorMsg);
          if (AppConfig.DEBUG_MODE) {
            parseException.printStackTrace(provider.getWriter());
          }
        }
      } else {
        // Should not happen for fields that need parsing, but if a field has no parser.
        parsedResults.put(field.getName(), stringValue);
      }
    }
    return parsedResults;
  }


  private List<FormField<?>> getFormFieldsRecursively(List<FormField<?>> initialFields,
      Map<String, ?> rawResults, // This contains PromptResultItemIF for JLine, String for Stdout
      boolean isJLine) {
    List<FormField<?>> allEffectivelyPromptedFields = new ArrayList<>();
    Queue<FormField<?>> fieldsToConsider = new LinkedList<>();
    Set<String> visitedFieldNames = new HashSet<>(); // Tracks fields added to allEffectivelyPromptedFields
    Set<String> enqueuedFieldNames = new HashSet<>(); // Tracks fields added to fieldsToConsider queue

    for (FormField<?> initialField : initialFields) {
      if (enqueuedFieldNames.add(initialField.getName())) {
        fieldsToConsider.add(initialField);
      }
    }

    while (!fieldsToConsider.isEmpty()) {
      FormField<?> currentField = fieldsToConsider.poll();

      // A field is only "prompted" if its name exists as a key in rawResults.
      // This means gatherJLineInput/gatherStdoutInput decided to prompt it AND got a result.
      if (!rawResults.containsKey(currentField.getName())) {
        continue;
      }

      if (visitedFieldNames.add(currentField.getName())) {
        allEffectivelyPromptedFields.add(currentField);
      } else {
        // Already processed and added to the final list, skip further processing of this instance.
        continue;
      }

      Map<String, List<FormField<?>>> conditionalChildrenMap = currentField.getConditionalChildren();
      // MODIFICATION: Changed condition here
      if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) {
        continue;
      }

      Object resultValue = rawResults.get(currentField.getName());
      if (resultValue == null) {
        continue;
      }

      String conditionalTriggerKey = null;
      if (isJLine) {
        // MODIFICATION: Logic to get conditional trigger key
        if (currentField instanceof EventField ef && ef.getResolvedConditionalKey() != null) {
          conditionalTriggerKey = ef.getResolvedConditionalKey();
        } else if (resultValue instanceof PromptResultItemIF pri) { // Fallback for CHOICES etc.
          conditionalTriggerKey = extractStringFromJLineResult(pri);
        }
      } else { // Stdout path
        // Stdout path for EventField's conditional children isn't fully handled by this fix,
        // as EventField on stdout doesn't currently set a resolvedConditionalKey.
        // This will use the direct string value (event ID for EventField), which is the old behavior.
        conditionalTriggerKey = (String) resultValue;
      }

      if (conditionalTriggerKey == null) {
        continue;
      }

      List<FormField<?>> children = conditionalChildrenMap.get(conditionalTriggerKey);
      if (children == null || children.isEmpty()) {
        continue;
      }

      for (FormField<?> child : children) {
        // Add to queue if not already visited or enqueued.
        // The check rawResults.containsKey(child.getName()) will happen when child becomes currentField.
        if (!visitedFieldNames.contains(child.getName()) &&
            enqueuedFieldNames.add(child.getName())) {
          fieldsToConsider.add(child);
        }
      }
    }
    return allEffectivelyPromptedFields;
  }

  // MODIFICATION: Made static as it doesn't depend on FormPage instance state
  private static String extractStringFromJLineResult(PromptResultItemIF item) {
    return switch (item) {
      case null -> null;
      case InputResult inputResult -> inputResult.getResult();
      case ListResult listResult -> listResult.getSelectedId();
      case ConfirmResult cr -> cr.getConfirmed() == ConfirmChoice.ConfirmationValue.YES ?
          Boolean.TRUE.toString() : Boolean.FALSE.toString();
      default -> item.getDisplayResult(); // Should cover other cases or be null if not applicable
    };
  }

  private Map<String, PromptResultItemIF> gatherJLineInput(
      JLineInteractionProvider jlp,
      List<FormField<?>> initialFields,
      Supplier<List<AttributedString>> headerSupplier) throws IOException, UserInterruptException {

    Map<String, PromptResultItemIF> allResults = new HashMap<>();
    Deque<FormField<?>> fieldsToProcess = new LinkedList<>(initialFields);
    Set<String> processedFieldNames = new HashSet<>(); // Tracks fields for which promptAndValidate has been called and result stored.
    List<AttributedString> dynamicErrorMessages = new ArrayList<>();

    while (!fieldsToProcess.isEmpty()) {
      FormField<?> field = fieldsToProcess.pollFirst();

      // If already processed (e.g. if it was added to queue multiple times by different parents but processed once)
      if (allResults.containsKey(field.getName())) {
        continue;
      }

      PromptResultItemIF resultItem = promptAndValidateJLineField(field, jlp,
          dynamicErrorMessages, headerSupplier, allResults); // allResults might be used for context by promptAndValidate (not currently)

      // promptAndValidateJLineField might throw UserInterruptException, caught by display()
      // If resultItem is null (e.g. UserInterruptException within the prompt itself, though usually it throws up)
      // or if the prompt somehow fails to return an item for this field, we might skip.
      // However, promptAndValidateJLineField is designed to loop until valid or interrupt.

      if (resultItem != null) {
        allResults.put(field.getName(), resultItem);
      }
      // Mark as processed regardless of null resultItem to avoid re-prompting the same field instance from the queue.
      // The crucial check is allResults.containsKey for parsing.
      processedFieldNames.add(field.getName());

      dynamicErrorMessages.clear(); // Clear errors for the next field

      Map<String, List<FormField<?>>> conditionalChildrenMap = field.getConditionalChildren();
      if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()
          || resultItem == null) { // No result, no conditional children based on it.
        continue;
      }

      // MODIFICATION: Logic to get conditional trigger key
      String conditionalTriggerKeyValue = null;
      if (field instanceof EventField ef && ef.getResolvedConditionalKey() != null) {
        conditionalTriggerKeyValue = ef.getResolvedConditionalKey();
      } else { // Fallback for CHOICES and other types
        conditionalTriggerKeyValue = extractStringFromJLineResult(resultItem);
      }

      if (conditionalTriggerKeyValue == null) {
        continue;
      }

      List<FormField<?>> children = conditionalChildrenMap.get(conditionalTriggerKeyValue);
      // Add children to the front of the queue to process them next.
      // Pass processedFieldNames to avoid re-adding fields that have already been through promptAndValidate.
      addConditionalChildrenToQueue(children, fieldsToProcess, processedFieldNames);
    }

    return allResults;
  }

  private PromptResultItemIF promptAndValidateJLineField(
      FormField<?> field,
      JLineInteractionProvider jlp,
      List<AttributedString> dynamicErrorMessages,
      Supplier<List<AttributedString>> staticHeaderSupplier,
      Map<String, PromptResultItemIF> currentResults) throws IOException, UserInterruptException {

    Terminal terminal = jlp.getTerminal();
    LineReader lineReader = jlp.getLineReader();
    ConsolePrompt consolePrompt;
    PromptResultItemIF resultItem = null;
    boolean validated = false;

    EnumSet<FieldType> typesNeedingStrValidation = EnumSet.of(
        FieldType.TEXT, FieldType.NUMBER, FieldType.CPF, FieldType.DATE, FieldType.DATETIME,
        FieldType.PERSON // EVENT also needs parsing but has custom validation flow
    );

    // Clear any stale state from previous validations of this field if it's re-prompted.
    if (field instanceof EventField ef) {
      ef.setResolvedConditionalKey(null);
    }

    if (field instanceof PersonField pf) { // Assuming PersonField might have similar state
      // pf.clearResolvedState(); // Example
    }

    while (!validated) {
      consolePrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
      PromptBuilder builder = consolePrompt.getPromptBuilder();
      PromptableElementIF jlineElement = buildJLinePromptElement(field, builder);

      if (jlineElement == null) {
        throw new IOException("Failed to build JLine prompt for field: " + field.getName());
      }

      List<AttributedString> currentHeader = new ArrayList<>(staticHeaderSupplier.get());
      currentHeader.addAll(dynamicErrorMessages); // Add errors from previous attempt for THIS field

      Map<String, PromptResultItemIF> resultMap;
      try {
        resultMap = consolePrompt.prompt(currentHeader, List.of(jlineElement));
      } catch (UserInterruptException exception) {
        Application.handleInterruption(); // Set global interruption flag
        throw exception; // Rethrow to be caught by display() or gatherJLineInput loop
      }

      dynamicErrorMessages.clear(); // Clear errors for THIS field before next validation attempt or exit

      if (resultMap == null || resultMap.isEmpty()) {
        throw new UserInterruptException("Prompt returned empty result for field: " + field.getName());
      }

      resultItem = resultMap.get(field.getName());

      if (resultItem == null) {
        // Should not happen if resultMap is not empty and element name matches.
        throw new IOException("JLine prompt returned no result item for field: " + field.getName());
      }

      // Field-specific validation and interaction (Person, Event)
      switch (field.getFieldType()) {
        case PERSON: {
          String currentCpfAttempt = extractStringFromJLineResult(resultItem);
          // Parser validation (format, length)
          try {
            field.getParser().apply(currentCpfAttempt); // This will throw if CPF format is wrong
          } catch (Exception exception) {
            dynamicErrorMessages.add(new AttributedString(
                exception.getMessage(), // Parser's message (e.g., "Invalid CPF length")
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            // Clear any potential state if PersonField had it
            if (field instanceof PersonField pf) { /* pf.setResolvedPersonKey(null); */ }
            continue; // Re-prompt current field (PERSON)
          }

          Function<String, Optional<Person>> retriever = null;
          if (field instanceof PersonField personFieldInstance) {
            retriever = personFieldInstance.getDisplayPersonRetrieverFunction();
          }

          if (retriever == null) { // Should be caught by system config checks
            dynamicErrorMessages.add(new AttributedString(
                "Misconfigured PERSON field: Cannot retrieve person details.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            throw new IllegalStateException("Field is PERSON type but retriever is unavailable for field: " + field.getName());
          }

          Optional<Person> personInfoOpt = retriever.apply(currentCpfAttempt); // currentCpfAttempt is raw input here, parser already ran
          // retriever should expect cleaned CPF if parser cleans it.
          // Let's assume parser returns cleaned CPF, and retriever expects that.
          String parsedCpfForRetriever;
          try {
            parsedCpfForRetriever = (String) field.getParser().apply(currentCpfAttempt);
          } catch (Exception e) { // Should not happen if first parse succeeded.
            parsedCpfForRetriever = currentCpfAttempt; // fallback, though problematic
          }
          personInfoOpt = retriever.apply(parsedCpfForRetriever);


          if (personInfoOpt.isEmpty()) {
            dynamicErrorMessages.add(new AttributedString(
                "Person with CPF " + currentCpfAttempt + " not found. Please try another CPF.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            if (field instanceof PersonField pf) { /* pf.setResolvedPersonKey(null); */ }
            continue; // Re-prompt current field (PERSON)
          }

          Person personInfo = personInfoOpt.get();
          // Confirmation prompt
          ConsolePrompt confirmPrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
          PromptBuilder confirmBuilder = confirmPrompt.getPromptBuilder();
          confirmBuilder.createConfirmPromp()
              .name("person_confirm")
              .message("Is this the correct person?")
              .defaultValue(ConfirmChoice.ConfirmationValue.YES)
              .addPrompt();

          Map<String, PromptResultItemIF> confirmResultMap;
          try {
            confirmResultMap = confirmPrompt.prompt(
                renderPersonFoundHeader(personInfo.getCpf(), personInfo.getName()),
                List.of(confirmBuilder.build().getFirst())
            );
          } catch (UserInterruptException exception) { // Interruption during confirmation
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation cancelled. Please enter CPF again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            if (field instanceof PersonField pf) { /* pf.setResolvedPersonKey(null); */ }
            continue; // Re-prompt main PERSON field
          }

          if (confirmResultMap == null || confirmResultMap.isEmpty() || !(confirmResultMap.get("person_confirm") instanceof ConfirmResult confirmValue)) {
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation failed. Please enter CPF again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            if (field instanceof PersonField pf) { /* pf.setResolvedPersonKey(null); */ }
            continue; // Re-prompt main PERSON field
          }

          if (confirmValue.getConfirmed() == ConfirmChoice.ConfirmationValue.YES) {
            validated = true; // CPF confirmed
            // if (field instanceof PersonField pf) { pf.setResolvedPersonKey(personInfo.getSomeKey()); } // Example
          } else { // User said "no" to person confirmation
            dynamicErrorMessages.add(new AttributedString(
                "Incorrect person. Please enter a different CPF.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            if (field instanceof PersonField pf) { /* pf.setResolvedPersonKey(null); */ }
            // No continue here, the outer while loop will re-prompt person_cpf because validated is false.
          }
          break; // Break from switch, outer while loop condition (validated) will handle next step.
        }

        case EVENT: {
          String currentEventIdAttempt = extractStringFromJLineResult(resultItem);
          EventField eventFieldInstance = null;
          if (field instanceof EventField ef) {
            eventFieldInstance = ef;
          }

          // Clear previous state for this field instance for this validation attempt
          if (eventFieldInstance != null) {
            eventFieldInstance.setResolvedConditionalKey(null);
          }

          try {
            // Parser validates format and existence (throws if not found or invalid format)
            field.getParser().apply(currentEventIdAttempt);
          } catch (Exception exception) { // Catches "not found", "invalid format" from parser
            dynamicErrorMessages.add(new AttributedString(
                exception.getMessage(),
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue; // Re-prompt current field (EVENT)
          }

          // Retriever for display after basic parsing/existence check
          Function<String, Optional<Event>> retriever = null;
          if (eventFieldInstance != null) {
            retriever = eventFieldInstance.getDisplayEventRetrieverFunction();
          }

          if (retriever == null) { // Should be caught by system config checks
            dynamicErrorMessages.add(new AttributedString(
                "Misconfigured EVENT field: Cannot retrieve event details.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            throw new IllegalStateException("Field is EVENT type but retriever is unavailable for field: " + field.getName());
          }

          // The parser already validated existence. Retriever is for display details.
          // We need the parsed Long ID for the retriever for consistency.
          Long parsedEventId;
          try {
            parsedEventId = (Long) field.getParser().apply(currentEventIdAttempt);
          } catch (Exception e) { // Should not happen if first parse succeeded.
            // This path indicates a severe issue if parser behaves differently on second call or type is wrong
            dynamicErrorMessages.add(new AttributedString(
                "Internal error processing event ID. Please try again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue;
          }
          // The retriever in EventField expects a String ID, let's ensure createInternalDisplayEventRetriever handles this.
          // It does: Long.parseLong(trimmedIdInput). So currentEventIdAttempt (string) is fine for it.
          Optional<Event> eventInfoOpt = retriever.apply(currentEventIdAttempt);


          // This check is technically redundant if parser guarantees existence.
          // However, keeping for safety or if retriever logic is complex.
          if (eventInfoOpt.isEmpty()) {
            dynamicErrorMessages.add(new AttributedString( // Should have been caught by parser
                "Event with ID '" + currentEventIdAttempt + "' not found. Please try another ID.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue; // Re-prompt current field (EVENT)
          }

          Event eventInfo = eventInfoOpt.get();

          // Confirmation prompt
          ConsolePrompt confirmPrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
          PromptBuilder confirmBuilder = confirmPrompt.getPromptBuilder();
          confirmBuilder.createConfirmPromp()
              .name("event_confirm")
              .message("Is this the correct event?")
              .defaultValue(ConfirmChoice.ConfirmationValue.YES)
              .addPrompt();

          Map<String, PromptResultItemIF> confirmResultMap;
          try {
            confirmResultMap = confirmPrompt.prompt(
                renderEventFoundHeader(eventInfo),
                List.of(confirmBuilder.build().getFirst())
            );
          } catch (UserInterruptException exception) {
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation cancelled. Please enter Event ID again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            // Do not set resolved key here, let the loop re-prompt.
            continue; // Re-prompt main EVENT field
          }

          if (confirmResultMap == null || confirmResultMap.isEmpty() || !(confirmResultMap.get("event_confirm") instanceof ConfirmResult confirmValue)) {
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation failed. Please enter Event ID again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            continue; // Re-prompt main EVENT field
          }

          if (confirmValue.getConfirmed() == ConfirmChoice.ConfirmationValue.YES) {
            validated = true; // Event ID confirmed
            if (eventFieldInstance != null) { // MODIFICATION: Set resolved key
              String triggerKey = eventInfo.getModality() != null ? eventInfo.getModality().name() : null;
              eventFieldInstance.setResolvedConditionalKey(triggerKey);
            }
            // MODIFICATION: REMOVED resultMap.put("modality", ...);
          } else { // User said "no" to event confirmation
            dynamicErrorMessages.add(new AttributedString(
                "Incorrect event. Please enter a different Event ID.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            // No continue here, validated is false, so outer while loop re-prompts event_id.
            // The resolvedConditionalKey remains null (or was reset at start of validation cycle for this field).
          }
          break; // Break from switch, outer while loop condition (validated) will handle next step.
        }

        default: { // For TEXT, NUMBER, DATE, DATETIME, CHOICE, BOOLEAN
          if (!typesNeedingStrValidation.contains(field.getFieldType()) && field.getFieldType() != FieldType.CHOICE && field.getFieldType() != FieldType.BOOLEAN) {
            // Fields not needing string validation (e.g. if some custom type didn't fit)
            // or already handled (CHOICE, BOOLEAN have their own JLine elements that handle selection)
            validated = true;
          } else if (field.getFieldType() == FieldType.CHOICE || field.getFieldType() == FieldType.BOOLEAN) {
            // For JLine's ListPrompt or ConfirmPrompt, the selection itself is considered valid.
            // The extractStringFromJLineResult will give the chosen ID or boolean string.
            // The parser for these (Enum.valueOf, Boolean.parseBoolean) might still fail if JLine somehow gives bad values,
            // but typically JLine constrains to valid choices.
            // If a parser exists, it will be applied in parseResults.
            // For validation loop, we trust JLine's element selection.
            validated = true;
          }
          else { // TEXT, NUMBER, CPF (already handled by PERSON but if used standalone), DATE, DATETIME
            String strValue = extractStringFromJLineResult(resultItem);

            if (field.getValidator().test(strValue)) { // Validator: basic format check
              try {
                if (field.getParser() != null) { // Parser: conversion and deeper validation
                  field.getParser().apply(strValue);
                }
                validated = true; // Both validator and parser passed
              } catch (Exception exceptionFromParser) { // Parser failed (e.g. date string parse error)
                String error = String.format(
                    "Invalid input for '%s'. %s. Error: %s",
                    field.getLabel(),
                    (exceptionFromParser.getMessage() != null && !exceptionFromParser.getMessage().equals(strValue)) ?
                        "" : // Parser message is useful
                        "Expected format: " + getFormatHint(field.getFieldType()), // Generic hint if parser msg is not helpful
                    exceptionFromParser.getMessage()
                );
                dynamicErrorMessages.add(new AttributedString(error,
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)));
              }
            } else { // Validator failed (e.g., regex mismatch)
              // Try to get a more specific message from the parser if validator fails.
              String error;
              try {
                if (field.getParser() != null) {
                  field.getParser().apply(strValue); // This might pass if validator is stricter or different.
                  // This call is just to try and get a parse error message.
                }
                // If parser didn't throw, but validator failed.
                error = "Invalid input for '" + field.getLabel()
                    + "'. Please adhere to the expected format/rules or the value is not acceptable (" + getFormatHint(field.getFieldType()) + ").";
              } catch (Exception exceptionFromParserAttempt) { // Parser also failed, its message is likely best.
                error = String.format("Invalid input for '%s'. %s. Error: %s",
                    field.getLabel(),
                    (exceptionFromParserAttempt.getMessage() != null && !exceptionFromParserAttempt.getMessage().equals(strValue)) ?
                        "" :
                        "Expected format: " + getFormatHint(field.getFieldType()),
                    exceptionFromParserAttempt.getMessage());
              }
              dynamicErrorMessages.add(new AttributedString(error,
                  AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)));
            }
          }
          break; // Break from switch.
        }
      } // End switch
    } // End while(!validated)
    return resultItem; // Return the (validated) result item for the current field.
  }

  private String getFormatHint(FieldType fieldType) {
    return switch (fieldType) {
      case DATE -> GlobalScope.DATE_FORMAT_STRING;
      case DATETIME -> GlobalScope.DATE_TIME_FORMAT_STRING;
      case CPF, PERSON -> "000.000.000-00";
      default -> "Valid " + fieldType.toString().toLowerCase();
    };
  }

  private PromptableElementIF buildJLinePromptElement(FormField<?> field,
      PromptBuilder promptBuilder) {
    String defaultValueStr = formatDefaultValueForDisplay(field, true);
    String message = field.getLabel();

    switch (field.getFieldType()) {
      case TEXT, NUMBER, EVENT -> // EVENT uses input prompt for ID
          promptBuilder.createInputPrompt().name(field.getName()).message(message + ":")
              .defaultValue(defaultValueStr).addPrompt();
      case CPF, PERSON -> promptBuilder.createInputPrompt().name(field.getName())
          .message(message + " (format: 000.000.000-00):")
          .defaultValue(defaultValueStr).addPrompt();
      case DATE -> promptBuilder.createInputPrompt().name(field.getName())
          .message(message + " (format: " + GlobalScope.DATE_FORMAT_STRING + "):")
          .defaultValue(defaultValueStr).addPrompt();
      case DATETIME -> promptBuilder.createInputPrompt().name(field.getName())
          .message(message + " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "):")
          .defaultValue(defaultValueStr).addPrompt();
      case CHOICE -> {
        var listPrompt = promptBuilder.createListPrompt()
            .name(field.getName()).message(field.getLabel() + ":");

        if (field.getOptions() != null) {
          field.getOptions()
              .forEach((value, display) -> listPrompt.newItem(value).text(display).add());
        }

        listPrompt.addPrompt();
      }
      case BOOLEAN -> {
        ConfirmChoice.ConfirmationValue defaultConfirm =
            (field.getDefaultValue() instanceof Boolean b && b) ? // Check actual boolean default
                ConfirmChoice.ConfirmationValue.YES : ConfirmChoice.ConfirmationValue.NO;
        promptBuilder.createConfirmPromp().name(field.getName()).message(field.getLabel())
            .defaultValue(defaultConfirm).addPrompt();
      }
    }

    List<PromptableElementIF> elements = promptBuilder.build();
    return (elements != null && !elements.isEmpty()) ? elements.getFirst() : null;
  }

  private Map<String, String> gatherStdoutInput(InteractionProvider provider,
      List<FormField<?>> initialFields) throws UserInterruptException {
    Map<String, String> results = new HashMap<>();
    Deque<FormField<?>> fieldsToProcess = new LinkedList<>(initialFields);
    Set<String> processedFieldNames = new HashSet<>(); // Tracks fields for which a value has been stored in results

    while (!fieldsToProcess.isEmpty()) {
      FormField<?> field = fieldsToProcess.pollFirst();

      if (results.containsKey(field.getName())) { // Already processed and has a result
        continue;
      }

      String value = promptAndValidateStdoutField(field, provider); // This can throw UserInterruptException

      // If promptAndValidateStdoutField returns null (e.g. due to unhandled parsing error within it, though it should loop)
      // or if an empty string is valid and returned, it will be put.
      // If UserInterruptException, it's caught by display().
      results.put(field.getName(), value); // Store the validated value
      processedFieldNames.add(field.getName());


      Map<String, List<FormField<?>>> conditionalChildrenMap = field.getConditionalChildren();
      if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) {
        continue;
      }

      // For stdout, the 'value' is the direct string result of the parent field.
      // This will work for CHOICE fields. For EventField, this 'value' is the event ID.
      // To make EventField's conditional children work on stdout, promptAndValidateStdoutField
      // would need to be enhanced for EVENT type to somehow resolve and store the modality string,
      // and this logic would need to retrieve it, similar to JLine's resolvedConditionalKey.
      // This is outside the scope of the immediate JLine fix.
      String conditionalTriggerKeyValue = value; // Current Stdout behavior

      if (conditionalTriggerKeyValue != null) {
        List<FormField<?>> children = conditionalChildrenMap.get(conditionalTriggerKeyValue);
        addConditionalChildrenToQueue(children, fieldsToProcess, processedFieldNames);
      }
    }
    return results;
  }

  private String promptAndValidateStdoutField(FormField<?> field, InteractionProvider provider)
      throws UserInterruptException {
    PrintWriter writer = provider.getWriter();
    String inputValue = null;
    boolean validInput = false;

    // For PersonField on Stdout - specific handling for CPF confirmation
    // This is simplified; a full implementation would mirror JLine's detail.
    String confirmedCpfForPersonField = null;


    while (!validInput) {
      // --- PERSON Field Special Handling for STDOUT (simplified) ---
      if (field.getFieldType() == FieldType.PERSON) {
        if (confirmedCpfForPersonField == null) { // Need to ask for CPF
          writer.print(field.getLabel() + " (format: 000.000.000-00): ");
          writer.flush();
          String rawLine = provider.readLine("").trim();

          if ("ctrl+c".equalsIgnoreCase(rawLine)) { // Simplified interrupt check
            throw new UserInterruptException("Form cancelled by user (Ctrl+C detected).");
          }

          // Validate CPF format (parser typically does this)
          try {
            String parsedCpf = (String) field.getParser().apply(rawLine); // Parser cleans and validates format
            // Now, simulate lookup and confirmation
            Function<String, Optional<Person>> retriever = ((PersonField)field).getDisplayPersonRetrieverFunction();
            Optional<Person> personOpt = retriever.apply(parsedCpf);

            if (personOpt.isPresent()) {
              Person person = personOpt.get();
              writer.println("\nPerson Found: " + person.getName() +
                  " (CPF: " + GlobalScope.CPF_FORMATTER.apply(person.getCpf()) + ")");
              writer.print("Is this the correct person? (yes/no, default: yes): ");
              writer.flush();
              String confirmLine = provider.readLine("").trim().toLowerCase();
              if ("ctrl+c".equalsIgnoreCase(confirmLine)) {
                throw new UserInterruptException("Form cancelled during PERSON confirmation.");
              }
              if (confirmLine.isEmpty() || confirmLine.equals("yes") || confirmLine.equals("y")) {
                confirmedCpfForPersonField = parsedCpf; // Store the successfully parsed and confirmed CPF
                inputValue = confirmedCpfForPersonField;
                validInput = true;
              } else {
                writer.println("Incorrect person. Please enter a different CPF.");
                // Loop will continue, confirmedCpfForPersonField is null, will re-ask.
              }
            } else {
              writer.println("Person with CPF " + rawLine + " not found. Please try again.");
            }
          } catch (IllegalArgumentException e) {
            writer.println("Invalid CPF: " + e.getMessage() + ". Please try again.");
          } catch (ClassCastException e) {
            writer.println("Configuration error with PersonField parser. Please contact admin.");
            throw new RuntimeException("PersonField parser did not return String", e);
          }
        } else {
          // This state (confirmedCpfForPersonField != null but validInput == false) shouldn't be reached
          // if logic above is correct. If it is, it's an error.
          // For safety, we set validInput = true if confirmedCpfForPersonField is already set.
          inputValue = confirmedCpfForPersonField;
          validInput = true;
        }
        if (validInput) continue; // Skip generic prompt if person handled
      }
      // --- End PERSON Field Special Handling ---


      // Generic prompt for other fields
      writer.print(field.getLabel());
      String defaultValueDisplay = formatDefaultValueForDisplay(field, false);
      if (defaultValueDisplay != null && !defaultValueDisplay.isEmpty()) {
        writer.print(" (default: " + defaultValueDisplay + ")");
      }
      writer.print(getStdoutPromptSuffix(field, writer)); // Handles choice display etc.
      writer.flush();

      String rawLine = provider.readLine("").trim();

      if ("ctrl+c".equalsIgnoreCase(rawLine)) { // Simplified interrupt check
        throw new UserInterruptException("Form cancelled by user (Ctrl+C detected).");
      }

      if (rawLine.isEmpty() && field.getDefaultValue() != null) {
        inputValue = getDefaultValueAsString(field);
        // For stdout, we assume default value is valid if provided.
        // A more robust way would be to validate it too, but often defaults are trusted.
        if (!field.getValidator().test(inputValue)) {
          writer.println("Warning: Default value ('" + inputValue + "') for '" +
              field.getLabel() + "' is considered invalid by validator, but used as per default empty input.");
        }
        validInput = true;
      } else {
        String processedInput = processStdoutRawInput(field, rawLine, writer); // Handles CHOICE number to key, BOOLEAN y/n to true/false
        if (processedInput == null) { // processStdoutRawInput returns null on input error (e.g. non-number for choice)
          continue; // Re-prompt
        }

        // Now validate the processedInput (which is the actual key for choice, or string for text)
        if (field.getValidator().test(processedInput)) {
          try {
            if (field.getParser() != null) {
              field.getParser().apply(processedInput); // Try parsing to catch parser-specific errors
            }
            inputValue = processedInput;
            validInput = true;
          } catch (Exception exceptionFromParser) { // Parser failed
            writer.println(
                "Invalid input for '" + field.getLabel() + "': " + exceptionFromParser.getMessage() +
                    ". Please try again.");
          }
        } else { // Validator failed
          // Try to get a message from parser if validator fails, similar to JLine.
          String errorMsg;
          try {
            if (field.getParser() != null) field.getParser().apply(processedInput);
            errorMsg = "Invalid input for '" + field.getLabel() + "'. Please try again."; // Generic if parser didn't throw
          } catch (Exception e) {
            errorMsg = "Invalid input for '" + field.getLabel() + "': " + e.getMessage() + ". Please try again.";
          }
          writer.println(errorMsg);
        }
      }
    } // End while(!validInput)
    return inputValue;
  }

  private String getStdoutPromptSuffix(FormField<?> field, PrintWriter writer) {
    return switch (field.getFieldType()) {
      case CHOICE -> {
        writer.println(); // Newline before options
        if (field.getOptions() != null) {
          List<String> keys = new ArrayList<>(field.getOptions().keySet()); // Ordered if LinkedHashMap
          for (int i = 0; i < keys.size(); i++) {
            writer.printf("  %d. %s%n", i + 1, field.getOptions().get(keys.get(i)));
          }
        }
        yield "Enter choice (number): ";
      }
      case BOOLEAN -> " (yes/no): ";
      case DATE -> " (format: " + GlobalScope.DATE_FORMAT_STRING + "): ";
      case DATETIME -> " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "): ";
      case CPF /* PERSON is handled separately */ -> " (format: 000.000.000-00): ";
      default -> ": ";
    };
  }

  private String processStdoutRawInput(FormField<?> field, String rawLine, PrintWriter writer) {
    return switch (field.getFieldType()) {
      case CHOICE -> {
        try {
          int choiceIdx = Integer.parseInt(rawLine) - 1; // User enters 1-based index
          if (field.getOptions() != null) {
            List<String> keys = new ArrayList<>(field.getOptions().keySet()); // Keys are enum names or similar
            if (choiceIdx >= 0 && choiceIdx < keys.size()) {
              yield keys.get(choiceIdx); // Return the actual key (e.g., "IN_PERSON")
            }
          }
          writer.println("Invalid choice number. Please try again.");
          yield null; // Indicate error to re-prompt
        } catch (NumberFormatException exception) {
          writer.println("Please enter a number for your choice. Please try again.");
          yield null; // Indicate error to re-prompt
        }
      }
      case BOOLEAN -> {
        if ("yes".equalsIgnoreCase(rawLine) || "y".equalsIgnoreCase(rawLine)) {
          yield Boolean.TRUE.toString();
        }
        if ("no".equalsIgnoreCase(rawLine) || "n".equalsIgnoreCase(rawLine)) {
          yield Boolean.FALSE.toString();
        }
        // If default value is used on empty input, this won't be hit for empty line.
        // If input is not empty and not y/n, then it's an invalid boolean string.
        writer.println("Please answer 'yes' or 'no'.");
        yield null; // Indicate error to re-prompt (unless validator/parser handles this message better)
      }
      default -> rawLine; // For text, number, date, etc., return raw input for validator/parser
    };
  }


  private String getDefaultValueAsString(FormField<?> field) {
    Object defVal = field.getDefaultValue();
    if (defVal == null) {
      return null;
    }

    return switch (defVal) {
      case Enum<?> enumVal when field.getFieldType() == FieldType.CHOICE -> enumVal.name(); // Use key for default
      case LocalDate ld when field.getFieldType() == FieldType.DATE ->
          GlobalScope.DATE_FORMAT.format(ld);
      case LocalDateTime ldt when field.getFieldType() == FieldType.DATETIME ->
          GlobalScope.DATE_TIME_FORMAT.format(ldt);
      case Boolean b -> b.toString(); // "true" or "false"
      default -> defVal.toString();
    };
  }

  private String formatDefaultValueForDisplay(FormField<?> field, boolean isJLineContext) {
    Object defVal = field.getDefaultValue();
    if (defVal == null) return null;

    FieldType fieldType = field.getFieldType();

    switch (defVal) {
      case LocalDate ld when fieldType == FieldType.DATE -> {
        return GlobalScope.DATE_FORMAT.format(ld);
      }
      case LocalDateTime ldt when fieldType == FieldType.DATETIME -> {
        return GlobalScope.DATE_TIME_FORMAT.format(ldt);
      }
      case Enum<?> enumVal when fieldType == FieldType.CHOICE -> {
        if (isJLineContext)
          return enumVal.name();
        return field.getOptions() != null ?
            field.getOptions().getOrDefault(enumVal.name(), enumVal.toString()) :
            enumVal.toString();
      }
      case Boolean bVal when fieldType == FieldType.BOOLEAN -> {
        if (isJLineContext)
          return bVal.toString();
        Map<String, String> options = field.getOptions();
        return options != null ?
            options.getOrDefault(bVal.toString(), bVal ? "yes" : "no") :
            (bVal ? "yes" : "no");
      }
      default -> {
        return defVal.toString();
      }
    }
  }

  private void addConditionalChildrenToQueue(List<FormField<?>> children,
      Deque<FormField<?>> fieldsToProcess,
      Set<String> processedOrEnqueuedFieldNames) { // Changed Set to reflect combined meaning
    if (children == null || children.isEmpty()) {
      return;
    }

    // Add children to the front of the deque to process them immediately after the parent.
    // Iterate in reverse to maintain original order when adding to front.
    for (int i = children.size() - 1; i >= 0; i--) {
      FormField<?> child = children.get(i);
      // Add if not already processed (in allResults) and not already in queue
      // processedOrEnqueuedFieldNames should track fields that are already in 'allResults' (from gatherJLineInput)
      // OR fields that are already in 'fieldsToProcess' queue.
      // The 'processedFieldNames' in gatherJLineInput tracks fields added to allResults.
      // Here, we check if it's already in the queue to avoid duplicates from complex conditions.
      // A simpler check might be just !fieldsToProcess.contains(child) if performance isn't an issue for queue size.
      // For now, using the passed set which should correctly track what's been handled or is pending.
      if (processedOrEnqueuedFieldNames.add(child.getName())) { // If add is true, it was not already in the set
        fieldsToProcess.addFirst(child);
      }
    }
  }

  private ArrayList<AttributedString> renderPersonFoundHeader(String cpf, String personInfo) {
    ArrayList<AttributedString> header = new ArrayList<>();
    header.add(new AttributedString("")); // Spacer
    header.add(new AttributedString("Person Found: " + personInfo,
        AttributedStyle.BOLD.foreground(AttributedStyle.GREEN)));
    header.add(new AttributedString(" • CPF: " + GlobalScope.CPF_FORMATTER.apply(cpf),
        AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(new AttributedString("")); // Spacer
    return header;
  }

  private ArrayList<AttributedString> renderEventFoundHeader(Event event) {
    ArrayList<AttributedString> header = new ArrayList<>();
    header.add(new AttributedString("")); // Spacer
    header.add(new AttributedString("Event Found: " + event.getTitle(),
        AttributedStyle.BOLD.foreground(AttributedStyle.GREEN)));
    header.add(new AttributedString(" • ID: #" + event.getId(),
        AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(new AttributedString(
        " • Type: " + (event.getType() != null ? event.getType().getLabel() : "N/A"),
        AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(
        new AttributedString(" • When: " + GlobalScope.DATE_TIME_FORMAT.format(event.getDate()),
            AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(new AttributedString(
        " • Modality: " + (event.getModality() != null ? event.getModality().getLabel() : "N/A"),
        AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(
        new AttributedString(" • Place: " + (event.getPlace() != null ? event.getPlace() : "N/A"),
            AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(new AttributedString("")); // Spacer
    return header;
  }
}