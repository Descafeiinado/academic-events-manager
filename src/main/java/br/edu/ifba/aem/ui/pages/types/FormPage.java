package br.edu.ifba.aem.ui.pages.types;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.models.DateRange;
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

  private static String extractStringFromJLineResult(PromptResultItemIF item) {
    return switch (item) {
      case null -> null;
      case InputResult inputResult -> inputResult.getResult();
      case ListResult listResult -> listResult.getSelectedId();
      case ConfirmResult cr -> cr.getConfirmed() == ConfirmChoice.ConfirmationValue.YES ?
          Boolean.TRUE.toString() : Boolean.FALSE.toString();
      default -> item.getDisplayResult();
    };
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
        if (!(rawValue instanceof PromptResultItemIF item)) {
          if (AppConfig.DEBUG_MODE) {
            provider.getWriter().println("Warning: JLine raw result for field '" + field.getName()
                + "' is not a PromptResultItemIF: " + (rawValue != null ? rawValue.getClass()
                .getName() : "null"));
          }
          continue;
        }

        if (field.getFieldType() == FieldType.BOOLEAN) {
          if (item instanceof ConfirmResult cr) {
            boolean confirmed = cr.getConfirmed() == ConfirmChoice.ConfirmationValue.YES;
            parsedResults.put(field.getName(), confirmed);
          }
          continue;
        }
        stringValue = extractStringFromJLineResult(item);
      } else {
        stringValue = (String) rawValue;
      }

      Function<String, ?> parser = field.getParser();

      if (parser != null) {
        try {
          parsedResults.put(field.getName(), parser.apply(stringValue));
        } catch (Exception parseException) {
          String errorMsg = String.format(
              "Error parsing value for field '%s': value was \"%s\". Error: %s",
              field.getLabel(), stringValue, parseException.getMessage());
          provider.getWriter().println(errorMsg);
          if (AppConfig.DEBUG_MODE) {
            parseException.printStackTrace(provider.getWriter());
          }
        }
      } else {
        parsedResults.put(field.getName(), stringValue);
      }
    }
    return parsedResults;
  }

  private List<FormField<?>> getFormFieldsRecursively(List<FormField<?>> initialFields,
      Map<String, ?> rawResults,
      boolean isJLine) {
    List<FormField<?>> allEffectivelyPromptedFields = new ArrayList<>();
    Queue<FormField<?>> fieldsToConsider = new LinkedList<>();
    Set<String> visitedFieldNames = new HashSet<>();
    Set<String> enqueuedFieldNames = new HashSet<>();

    for (FormField<?> initialField : initialFields) {
      if (enqueuedFieldNames.add(initialField.getName())) {
        fieldsToConsider.add(initialField);
      }
    }

    while (!fieldsToConsider.isEmpty()) {
      FormField<?> currentField = fieldsToConsider.poll();

      if (!rawResults.containsKey(currentField.getName())) {
        continue;
      }

      if (visitedFieldNames.add(currentField.getName())) {
        allEffectivelyPromptedFields.add(currentField);
      } else {
        continue;
      }

      Map<String, List<FormField<?>>> conditionalChildrenMap = currentField.getConditionalChildren();
      if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) {
        continue;
      }

      Object resultValue = rawResults.get(currentField.getName());
      if (resultValue == null) {
        continue;
      }

      String conditionalTriggerKey;
      if (currentField instanceof EventField ef && ef.getResolvedConditionalKey() != null) {
        conditionalTriggerKey = ef.getResolvedConditionalKey();
      } else {
        if (isJLine) {
          conditionalTriggerKey = extractStringFromJLineResult((PromptResultItemIF) resultValue);
        } else {
          conditionalTriggerKey = (String) resultValue;
        }
      }

      if (conditionalTriggerKey == null) {
        continue;
      }

      List<FormField<?>> children = conditionalChildrenMap.get(conditionalTriggerKey);
      if (children == null || children.isEmpty()) {
        continue;
      }

      for (FormField<?> child : children) {
        if (!visitedFieldNames.contains(child.getName()) &&
            enqueuedFieldNames.add(child.getName())) {
          fieldsToConsider.add(child);
        }
      }
    }
    return allEffectivelyPromptedFields;
  }

  private Map<String, PromptResultItemIF> gatherJLineInput(
      JLineInteractionProvider jlp,
      List<FormField<?>> initialFields,
      Supplier<List<AttributedString>> headerSupplier) throws IOException, UserInterruptException {

    Map<String, PromptResultItemIF> allResults = new HashMap<>();
    Deque<FormField<?>> fieldsToProcess = new LinkedList<>(initialFields);
    Set<String> processedFieldNames = new HashSet<>();
    List<AttributedString> dynamicErrorMessages = new ArrayList<>();

    while (!fieldsToProcess.isEmpty()) {
      FormField<?> field = fieldsToProcess.pollFirst();

      if (allResults.containsKey(field.getName())) {
        continue;
      }

      PromptResultItemIF resultItem = promptAndValidateJLineField(field, jlp,
          dynamicErrorMessages, headerSupplier);

      if (resultItem != null) {
        allResults.put(field.getName(), resultItem);
      }
      processedFieldNames.add(field.getName());

      dynamicErrorMessages.clear();

      Map<String, List<FormField<?>>> conditionalChildrenMap = field.getConditionalChildren();
      if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()
          || resultItem == null) {
        continue;
      }

      String conditionalTriggerKeyValue;
      if (field instanceof EventField ef && ef.getResolvedConditionalKey() != null) {
        conditionalTriggerKeyValue = ef.getResolvedConditionalKey();
      } else {
        conditionalTriggerKeyValue = extractStringFromJLineResult(resultItem);
      }

      if (conditionalTriggerKeyValue == null) {
        continue;
      }

      List<FormField<?>> children = conditionalChildrenMap.get(conditionalTriggerKeyValue);
      addConditionalChildrenToQueue(children, fieldsToProcess, processedFieldNames);
    }

    return allResults;
  }

  private PromptResultItemIF promptAndValidateJLineField(
      FormField<?> field,
      JLineInteractionProvider jlp,
      List<AttributedString> dynamicErrorMessages,
      Supplier<List<AttributedString>> staticHeaderSupplier)
      throws IOException, UserInterruptException {

    Terminal terminal = jlp.getTerminal();
    LineReader lineReader = jlp.getLineReader();
    ConsolePrompt consolePrompt;
    PromptResultItemIF resultItem = null;
    boolean validated = false;

    EnumSet<FieldType> typesNeedingStrValidation = EnumSet.of(
        FieldType.TEXT, FieldType.NUMBER, FieldType.CPF, FieldType.DATE, FieldType.DATETIME,
        FieldType.PERSON, FieldType.DATERANGE
    );

    if (field instanceof EventField eventField) {
      eventField.setResolvedConditionalKey(null);
    }

    while (!validated) {
      consolePrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
      PromptBuilder builder = consolePrompt.getPromptBuilder();
      PromptableElementIF jlineElement = buildJLinePromptElement(field, builder);

      if (jlineElement == null) {
        throw new IOException("Failed to build JLine prompt for field: " + field.getName());
      }

      List<AttributedString> currentHeader = new ArrayList<>(staticHeaderSupplier.get());
      currentHeader.addAll(dynamicErrorMessages);

      Map<String, PromptResultItemIF> resultMap;
      try {
        resultMap = consolePrompt.prompt(currentHeader, List.of(jlineElement));
      } catch (UserInterruptException exception) {
        Application.handleInterruption();
        throw exception;
      }

      dynamicErrorMessages.clear();

      if (resultMap == null || resultMap.isEmpty()) {
        throw new UserInterruptException(
            "Prompt returned empty result for field: " + field.getName());
      }

      resultItem = resultMap.get(field.getName());

      if (resultItem == null) {
        throw new IOException("JLine prompt returned no result item for field: " + field.getName());
      }

      switch (field.getFieldType()) {
        case PERSON: {
          String currentCpfAttempt = extractStringFromJLineResult(resultItem);
          try {
            field.getParser().apply(currentCpfAttempt);
          } catch (Exception exception) {
            dynamicErrorMessages.add(new AttributedString(
                exception.getMessage(),
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue;
          }

          Function<String, Optional<Person>> retriever = null;
          if (field instanceof PersonField personFieldInstance) {
            retriever = personFieldInstance.getDisplayPersonRetrieverFunction();
          }

          if (retriever == null) {
            dynamicErrorMessages.add(new AttributedString(
                "Misconfigured PERSON field: Cannot retrieve person details.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            throw new IllegalStateException(
                "Field is PERSON type but retriever is unavailable for field: " + field.getName());
          }

          retriever.apply(
              currentCpfAttempt);

          Optional<Person> personInfoOpt;
          String parsedCpfForRetriever;

          try {
            parsedCpfForRetriever = (String) field.getParser().apply(currentCpfAttempt);
          } catch (Exception e) {
            parsedCpfForRetriever = currentCpfAttempt;
          }
          personInfoOpt = retriever.apply(parsedCpfForRetriever);

          if (personInfoOpt.isEmpty()) {
            dynamicErrorMessages.add(new AttributedString(
                "Person with CPF " + currentCpfAttempt + " not found. Please try another CPF.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue;
          }

          Person personInfo = personInfoOpt.get();
          ConsolePrompt confirmPrompt = new ConsolePrompt(lineReader, terminal,
              AppConfig.UI_CONFIG);
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
          } catch (UserInterruptException exception) {
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation cancelled. Please enter CPF again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            continue;
          }

          if (confirmResultMap == null || confirmResultMap.isEmpty() || !(confirmResultMap.get(
              "person_confirm") instanceof ConfirmResult confirmValue)) {
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation failed. Please enter CPF again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            continue;
          }

          if (confirmValue.getConfirmed() == ConfirmChoice.ConfirmationValue.YES) {
            validated = true;
          } else {
            dynamicErrorMessages.add(new AttributedString(
                "Incorrect person. Please enter a different CPF.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
          }
          break;
        }

        case EVENT: {
          String currentEventIdAttempt = extractStringFromJLineResult(resultItem);
          EventField eventFieldInstance = null;
          if (field instanceof EventField ef) {
            eventFieldInstance = ef;
          }

          if (eventFieldInstance != null) {
            eventFieldInstance.setResolvedConditionalKey(null);
          }

          try {
            field.getParser().apply(currentEventIdAttempt);
          } catch (Exception exception) {
            dynamicErrorMessages.add(new AttributedString(
                exception.getMessage(),
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue;
          }

          Function<String, Optional<Event>> retriever = null;
          if (eventFieldInstance != null) {
            retriever = eventFieldInstance.getDisplayEventRetrieverFunction();
          }

          if (retriever == null) {
            dynamicErrorMessages.add(new AttributedString(
                "Misconfigured EVENT field: Cannot retrieve event details.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            throw new IllegalStateException(
                "Field is EVENT type but retriever is unavailable for field: " + field.getName());
          }

          try {
            field.getParser().apply(currentEventIdAttempt);
          } catch (Exception e) {
            dynamicErrorMessages.add(new AttributedString(
                "Internal error processing event ID. Please try again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue;
          }
          Optional<Event> eventInfoOpt = retriever.apply(currentEventIdAttempt);

          if (eventInfoOpt.isEmpty()) {
            dynamicErrorMessages.add(new AttributedString(
                "Event with ID '" + currentEventIdAttempt + "' not found. Please try another ID.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)
            ));
            continue;
          }

          Event eventInfo = eventInfoOpt.get();

          ConsolePrompt confirmPrompt = new ConsolePrompt(lineReader, terminal,
              AppConfig.UI_CONFIG);
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
            continue;
          }

          if (confirmResultMap == null || confirmResultMap.isEmpty() || !(confirmResultMap.get(
              "event_confirm") instanceof ConfirmResult confirmValue)) {
            dynamicErrorMessages.add(new AttributedString(
                "Confirmation failed. Please enter Event ID again.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
            continue;
          }

          if (confirmValue.getConfirmed() == ConfirmChoice.ConfirmationValue.YES) {
            validated = true;
            String triggerKey =
                eventInfo.getModality() != null ? eventInfo.getModality().name() : null;
            eventFieldInstance.setResolvedConditionalKey(triggerKey);
          } else {
            dynamicErrorMessages.add(new AttributedString(
                "Incorrect event. Please enter a different Event ID.",
                AttributedStyle.DEFAULT.foreground(AttributedStyle.YELLOW)
            ));
          }
          break;
        }

        default: {
          if (!typesNeedingStrValidation.contains(field.getFieldType())
              && field.getFieldType() != FieldType.CHOICE
              && field.getFieldType() != FieldType.BOOLEAN) {
            validated = true;
          } else if (field.getFieldType() == FieldType.CHOICE
              || field.getFieldType() == FieldType.BOOLEAN) {
            validated = true;
          } else {
            String strValue = extractStringFromJLineResult(resultItem);

            if (field.getValidator().test(strValue)) {
              try {
                if (field.getParser() != null) {
                  field.getParser().apply(strValue);
                }
                validated = true;
              } catch (
                  Exception exceptionFromParser) {
                String error = String.format(
                    "Invalid input for '%s'. %s. Error: %s",
                    field.getLabel(),
                    (exceptionFromParser.getMessage() != null && !exceptionFromParser.getMessage()
                        .equals(strValue)) ?
                        "" :
                        "Expected format: " + getFormatHint(field.getFieldType()),
                    exceptionFromParser.getMessage()
                );
                dynamicErrorMessages.add(new AttributedString(error,
                    AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)));
              }
            } else {
              String error;
              try {
                if (field.getParser() != null) {
                  field.getParser()
                      .apply(strValue);
                }
                error = "Invalid input for '" + field.getLabel()
                    + "'. Please adhere to the expected format/rules or the value is not acceptable ("
                    + getFormatHint(field.getFieldType()) + ").";
              } catch (
                  Exception exceptionFromParserAttempt) {
                error = String.format("Invalid input for '%s'. %s. Error: %s",
                    field.getLabel(),
                    (exceptionFromParserAttempt.getMessage() != null
                        && !exceptionFromParserAttempt.getMessage().equals(strValue)) ?
                        "" :
                        "Expected format: " + getFormatHint(field.getFieldType()),
                    exceptionFromParserAttempt.getMessage());
              }
              dynamicErrorMessages.add(new AttributedString(error,
                  AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)));
            }
          }
          break;
        }
      }
    }
    return resultItem;
  }

  private String getFormatHint(FieldType fieldType) {
    return switch (fieldType) {
      case DATE -> GlobalScope.DATE_FORMAT_STRING;
      case DATETIME -> GlobalScope.DATE_TIME_FORMAT_STRING;
      case CPF, PERSON -> "000.000.000-00";
      case DATERANGE -> DateRange.DATE_RANGE_FORMAT;
      default -> "Valid " + fieldType.toString().toLowerCase();
    };
  }

  private PromptableElementIF buildJLinePromptElement(FormField<?> field,
      PromptBuilder promptBuilder) {
    String defaultValueStr = formatDefaultValueForDisplay(field, true);
    String message = field.getLabel();

    switch (field.getFieldType()) {
      case TEXT, NUMBER, EVENT ->
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
      case DATERANGE -> promptBuilder.createInputPrompt().name(field.getName())
          .message(message + " (format: " + DateRange.DATE_RANGE_FORMAT + "):")
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
            (field.getDefaultValue() instanceof Boolean b && b) ?
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
    Set<String> processedFieldNames = new HashSet<>();

    while (!fieldsToProcess.isEmpty()) {
      FormField<?> field = fieldsToProcess.pollFirst();

      if (results.containsKey(field.getName())) {
        continue;
      }

      String value = promptAndValidateStdoutField(field, provider);

      results.put(field.getName(), value);
      processedFieldNames.add(field.getName());

      Map<String, List<FormField<?>>> conditionalChildrenMap = field.getConditionalChildren();
      if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) {
        continue;
      }

      String conditionalTriggerKey;
      if (field instanceof EventField ef && ef.getResolvedConditionalKey() != null) {
        conditionalTriggerKey = ef.getResolvedConditionalKey();
      } else {
        conditionalTriggerKey = value;
      }

      if (conditionalTriggerKey != null) {
        List<FormField<?>> children = conditionalChildrenMap.get(conditionalTriggerKey);
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

    if (field instanceof EventField ef) {
      ef.setResolvedConditionalKey(null);
    }

    while (!validInput) {
      if ("ctrl+c".equalsIgnoreCase(inputValue)) {
        throw new UserInterruptException("Form cancelled by user (Ctrl+C detected).");
      }
      switch (field.getFieldType()) {
        case PERSON: {
          writer.print(field.getLabel() + " (format: 000.000.000-00): ");
          writer.flush();
          String rawCpf = provider.readLine("").trim();
          if ("ctrl+c".equalsIgnoreCase(rawCpf)) {
            throw new UserInterruptException("Cancelled.");
          }

          try {
            String parsedCpf = (String) field.getParser().apply(rawCpf);

            assert field instanceof PersonField;

            var retriever = ((PersonField) field).getDisplayPersonRetrieverFunction();
            Optional<Person> personOpt = retriever.apply(parsedCpf);

            if (personOpt.isPresent()) {
              Person person = personOpt.get();
              writer.println("\n--- Person Found ---");
              writer.println("  Name: " + person.getName());
              writer.println("  CPF: " + GlobalScope.CPF_FORMATTER.apply(person.getCpf()));
              writer.println("--------------------");
              writer.print("Is this the correct person? (yes/no, default: yes): ");
              writer.flush();
              String confirmLine = provider.readLine("").trim().toLowerCase();
              if ("ctrl+c".equalsIgnoreCase(confirmLine)) {
                throw new UserInterruptException("Cancelled.");
              }

              if (confirmLine.isEmpty() || confirmLine.equals("yes") || confirmLine.equals("y")) {
                inputValue = parsedCpf;
                validInput = true;
              } else {
                writer.println("Incorrect person. Please enter a different CPF.\n");
              }
            } else {
              writer.println("Person with CPF " + rawCpf + " not found. Please try again.\n");
            }
          } catch (Exception e) {
            writer.println("Invalid input: " + e.getMessage() + ". Please try again.\n");
          }
          continue;
        }

        case EVENT: {
          writer.print(field.getLabel() + ": ");
          writer.flush();
          String rawId = provider.readLine("").trim();
          if ("ctrl+c".equalsIgnoreCase(rawId)) {
            throw new UserInterruptException("Cancelled.");
          }

          try {

            assert field instanceof EventField;

            var eventField = (EventField) field;
            var retriever = eventField.getDisplayEventRetrieverFunction();
            Optional<Event> eventOpt = retriever.apply(rawId);

            if (eventOpt.isPresent()) {
              Event event = eventOpt.get();
              writer.println("\n--- Event Found ---");
              writer.println("  Title: " + event.getTitle());
              writer.println("  ID: #" + event.getId());
              writer.println(
                  "  Type: " + (event.getType() != null ? event.getType().getLabel() : "N/A"));
              writer.println(
                  "  When: " + GlobalScope.DATE_TIME_FORMAT.format(event.getDate()));
              writer.println("  Modality: " + (event.getModality() != null ? event.getModality()
                  .getLabel() : "N/A"));
              writer.println("-------------------");
              writer.print("Is this the correct event? (yes/no, default: yes): ");
              writer.flush();
              String confirmLine = provider.readLine("").trim().toLowerCase();
              if ("ctrl+c".equalsIgnoreCase(confirmLine)) {
                throw new UserInterruptException("Cancelled.");
              }

              if (confirmLine.isEmpty() || confirmLine.equals("yes") || confirmLine.equals("y")) {
                inputValue = rawId;
                String triggerKey =
                    event.getModality() != null ? event.getModality().name() : null;
                eventField.setResolvedConditionalKey(triggerKey);
                validInput = true;
              } else {
                writer.println("Incorrect event. Please enter a different ID.\n");
              }
            } else {
              writer.println("Event with ID '" + rawId + "' not found. Please try again.\n");
            }
          } catch (Exception e) {
            writer.println("Invalid input: " + e.getMessage() + ". Please try again.\n");
          }
          continue;
        }

        default: {
          writer.print(field.getLabel());
          String defaultValueDisplay = formatDefaultValueForDisplay(field, false);
          if (defaultValueDisplay != null && !defaultValueDisplay.isEmpty()) {
            writer.print(" (default: " + defaultValueDisplay + ")");
          }
          writer.print(getStdoutPromptSuffix(field, writer));
          writer.flush();

          String rawLine = provider.readLine("").trim();
          if ("ctrl+c".equalsIgnoreCase(rawLine)) {
            throw new UserInterruptException("Form cancelled by user.");
          }

          if (rawLine.isEmpty() && field.getDefaultValue() != null) {
            inputValue = getDefaultValueAsString(field);
            validInput = true;
          } else {
            String processedInput = processStdoutRawInput(field, rawLine, writer);
            if (processedInput == null) {
              continue;
            }

            if (field.getValidator().test(processedInput)) {
              try {
                if (field.getParser() != null) {
                  field.getParser().apply(processedInput);
                }
                inputValue = processedInput;
                validInput = true;
              } catch (Exception exceptionFromParser) {
                writer.println("Invalid input for '" + field.getLabel() + "': "
                    + exceptionFromParser.getMessage() + ". Please try again.");
              }
            } else {
              writer.println(
                  "Invalid input format for '" + field.getLabel() + "'. Please try again.");
            }
          }
        }
      }
    }
    return inputValue;
  }


  private String getStdoutPromptSuffix(FormField<?> field, PrintWriter writer) {
    return switch (field.getFieldType()) {
      case CHOICE -> {
        writer.println();
        if (field.getOptions() != null) {
          List<String> keys = new ArrayList<>(
              field.getOptions().keySet());
          for (int i = 0; i < keys.size(); i++) {
            writer.printf("  %d. %s%n", i + 1, field.getOptions().get(keys.get(i)));
          }
        }
        yield "Enter choice (number): ";
      }
      case BOOLEAN -> " (yes/no): ";
      case DATE -> " (format: " + GlobalScope.DATE_FORMAT_STRING + "): ";
      case DATETIME -> " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "): ";
      case DATERANGE -> " (format: " + DateRange.DATE_RANGE_FORMAT + "): ";
      case CPF -> " (format: 000.000.000-00): ";
      default -> ": ";
    };
  }

  private String processStdoutRawInput(FormField<?> field, String rawLine, PrintWriter writer) {
    return switch (field.getFieldType()) {
      case CHOICE -> {
        try {
          int choiceIdx = Integer.parseInt(rawLine) - 1;
          if (field.getOptions() != null) {
            List<String> keys = new ArrayList<>(
                field.getOptions().keySet());
            if (choiceIdx >= 0 && choiceIdx < keys.size()) {
              yield keys.get(choiceIdx);
            }
          }
          writer.println("Invalid choice number. Please try again.");
          yield null;
        } catch (NumberFormatException exception) {
          writer.println("Please enter a number for your choice. Please try again.");
          yield null;
        }
      }
      case BOOLEAN -> {
        if ("yes".equalsIgnoreCase(rawLine) || "y".equalsIgnoreCase(rawLine)) {
          yield Boolean.TRUE.toString();
        }
        if ("no".equalsIgnoreCase(rawLine) || "n".equalsIgnoreCase(rawLine)) {
          yield Boolean.FALSE.toString();
        }
        writer.println("Please answer 'yes' or 'no'.");
        yield null;
      }
      default -> rawLine;
    };
  }


  private String getDefaultValueAsString(FormField<?> field) {
    Object defVal = field.getDefaultValue();
    if (defVal == null) {
      return null;
    }

    return switch (defVal) {
      case Enum<?> enumVal when field.getFieldType() == FieldType.CHOICE -> enumVal.name();
      case LocalDate ld when field.getFieldType() == FieldType.DATE ->
          GlobalScope.DATE_FORMAT.format(ld);
      case LocalDateTime ldt when field.getFieldType() == FieldType.DATETIME ->
          GlobalScope.DATE_TIME_FORMAT.format(ldt);
      case DateRange dr when field.getFieldType() == FieldType.DATERANGE ->
          dr.toString();
      case Boolean b -> b.toString();
      default -> defVal.toString();
    };
  }

  private String formatDefaultValueForDisplay(FormField<?> field, boolean isJLineContext) {
    Object defVal = field.getDefaultValue();
    if (defVal == null) {
      return null;
    }

    FieldType fieldType = field.getFieldType();

    return switch (defVal) {
      case LocalDate ld when fieldType == FieldType.DATE -> GlobalScope.DATE_FORMAT.format(ld);
      case LocalDateTime ldt when fieldType == FieldType.DATETIME ->
          GlobalScope.DATE_TIME_FORMAT.format(ldt);
      case DateRange dr when fieldType == FieldType.DATERANGE ->
          dr.toString();
      case Enum<?> enumVal when fieldType == FieldType.CHOICE -> isJLineContext ? enumVal.name() :
          field.getOptions() != null ? field.getOptions()
              .getOrDefault(enumVal.name(), enumVal.toString()) : enumVal.toString();
      case Boolean bVal when fieldType == FieldType.BOOLEAN -> {
        if (isJLineContext) {
          yield bVal.toString();
        }
        Map<String, String> options = field.getOptions();
        yield options != null ?
            options.getOrDefault(bVal.toString(), bVal ? "yes" : "no") :
            (bVal ? "yes" : "no");
      }
      default -> defVal.toString();
    };
  }

  private void addConditionalChildrenToQueue(List<FormField<?>> children,
      Deque<FormField<?>> fieldsToProcess,
      Set<String> processedOrEnqueuedFieldNames) {
    if (children == null || children.isEmpty()) {
      return;
    }

    for (int i = children.size() - 1; i >= 0; i--) {
      FormField<?> child = children.get(i);
      if (processedOrEnqueuedFieldNames.add(
          child.getName())) {
        fieldsToProcess.addFirst(child);
      }
    }
  }

  private ArrayList<AttributedString> renderPersonFoundHeader(String cpf, String personInfo) {
    ArrayList<AttributedString> header = new ArrayList<>();
    header.add(new AttributedString(""));
    header.add(new AttributedString("Person Found: " + personInfo,
        AttributedStyle.BOLD.foreground(AttributedStyle.GREEN)));
    header.add(new AttributedString(" • CPF: " + GlobalScope.CPF_FORMATTER.apply(cpf),
        AttributedStyle.DEFAULT.foreground(AttributedStyle.WHITE)));
    header.add(new AttributedString(""));
    return header;
  }

  private ArrayList<AttributedString> renderEventFoundHeader(Event event) {
    ArrayList<AttributedString> header = new ArrayList<>();
    header.add(new AttributedString(""));
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
    header.add(new AttributedString(""));
    return header;
  }

}