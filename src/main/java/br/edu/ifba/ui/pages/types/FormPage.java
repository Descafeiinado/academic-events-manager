package br.edu.ifba.ui.pages.types;

import br.edu.ifba.AppConfig;
import br.edu.ifba.GlobalScope;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.form.FormField;
import br.edu.ifba.ui.components.form.FieldType;
import br.edu.ifba.ui.pages.Page;
import br.edu.ifba.ui.providers.JLineInteractionProvider;
import br.edu.ifba.ui.providers.StdoutInteractionProvider;
import org.jline.consoleui.elements.ConfirmChoice;
import org.jline.consoleui.elements.PromptableElementIF;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.consoleui.prompt.ConfirmResult;
import org.jline.consoleui.prompt.InputResult;
import org.jline.consoleui.prompt.ListResult;
import org.jline.consoleui.prompt.PromptResultItemIF;
import org.jline.consoleui.prompt.builder.PromptBuilder;
import org.jline.reader.LineReader;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStyle;

import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Function;
import java.util.function.Supplier;

public abstract class FormPage extends Page {

    public FormPage(String title) {
        super(title);
    }

    protected abstract List<FormField<?>> getFormFields();

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
            List<FormField<?>> allPromptedFields = getFormFieldsRecursively(initialFields, rawResults, isJLine);
            Map<String, Object> parsedResults = parseResults(provider, rawResults, allPromptedFields);

            onSubmit(provider, parsedResults);
        } catch (UserInterruptException e) {
            provider.getWriter().println("\nForm cancelled by user.");
            onSubmit(provider, Collections.emptyMap());
        } catch (Exception e) {
            provider.getWriter().println("An error occurred during form processing: " + e.getMessage());
            if (AppConfig.DEBUG_MODE) {
                e.printStackTrace(provider.getWriter());
            }
            onSubmit(provider, Collections.emptyMap());
        }
    }

    private boolean isJLineProvider(InteractionProvider provider) {
        return provider.getNativeProvider() instanceof JLineInteractionProvider;
    }

    private Map<String, ?> gatherRawInputs(InteractionProvider provider, List<FormField<?>> initialFields)
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
            if (rawValue == null) continue;

            String stringValue;
            if (isJLine) {
                PromptResultItemIF item = (PromptResultItemIF) rawValue;
                if (field.getFieldType() == FieldType.BOOLEAN && item instanceof ConfirmResult cr) {
                    boolean confirmed = cr.getConfirmed() == ConfirmChoice.ConfirmationValue.YES;
                    parsedResults.put(field.getName(), confirmed);
                    continue;
                }
                stringValue = extractStringFromJLineResult(item);
            } else {
                stringValue = (String) rawValue;
            }

            if (stringValue == null) continue;

            Function<String, ?> parser = field.getParser();
            if (parser != null) {
                try {
                    parsedResults.put(field.getName(), parser.apply(stringValue));
                } catch (Exception parseException) {
                    String errorMsg = String.format("Error parsing value for field '%s': \"%s\". Error: %s",
                            field.getLabel(), stringValue, parseException.getMessage());
                    provider.getWriter().println(errorMsg);
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
            enqueuedFieldNames.remove(currentField.getName());

            if (visitedFieldNames.contains(currentField.getName())) continue;

            boolean isPresentInInitialFieldsList = initialFields.stream()
                    .anyMatch(f -> f.getName().equals(currentField.getName()));

            if (!rawResults.containsKey(currentField.getName()) && !isPresentInInitialFieldsList) {
                continue;
            }

            allEffectivelyPromptedFields.add(currentField);
            visitedFieldNames.add(currentField.getName());

            if (currentField.getFieldType() != FieldType.CHOICE) continue;
            Map<String, List<FormField<?>>> conditionalChildrenMap = currentField.getConditionalChildren();
            if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) continue;

            Object resultValue = rawResults.get(currentField.getName());
            if (resultValue == null) continue;

            String selectedChoiceKey = isJLine ?
                    extractStringFromJLineResult((PromptResultItemIF) resultValue) :
                    (String) resultValue;
            if (selectedChoiceKey == null) continue;

            List<FormField<?>> children = conditionalChildrenMap.get(selectedChoiceKey);
            if (children == null || children.isEmpty()) continue;

            for (FormField<?> child : children) {
                if (!visitedFieldNames.contains(child.getName()) &&
                        enqueuedFieldNames.add(child.getName())) {
                    fieldsToConsider.add(child);
                }
            }
        }
        return allEffectivelyPromptedFields;
    }

    private String extractStringFromJLineResult(PromptResultItemIF item) {
        return switch (item) {
            case null -> null;
            case InputResult inputResult -> inputResult.getResult();
            case ListResult listResult -> listResult.getSelectedId();
            case ConfirmResult cr -> cr.getConfirmed() == ConfirmChoice.ConfirmationValue.YES ?
                    Boolean.TRUE.toString() : Boolean.FALSE.toString();
            default -> item.getDisplayResult();
        };
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
            if (processedFieldNames.contains(field.getName())) continue;

            PromptResultItemIF resultItem = promptAndValidateJLineField(field, jlp,
                    dynamicErrorMessages, headerSupplier);
            allResults.put(field.getName(), resultItem);
            processedFieldNames.add(field.getName());

            dynamicErrorMessages.clear();

            Map<String, List<FormField<?>>> conditionalChildrenMap = field.getConditionalChildren();
            if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) continue;

            String selectedValue = extractStringFromJLineResult(resultItem);
            if (selectedValue == null) continue;

            List<FormField<?>> children = conditionalChildrenMap.get(selectedValue);
            addConditionalChildrenToQueue(children, fieldsToProcess, processedFieldNames);
        }
        return allResults;
    }

    private PromptResultItemIF promptAndValidateJLineField(
            FormField<?> field,
            JLineInteractionProvider jlp,
            List<AttributedString> dynamicErrorMessages,
            Supplier<List<AttributedString>> staticHeaderSupplier) throws IOException, UserInterruptException {

        Terminal terminal = jlp.getTerminal();
        LineReader lineReader = jlp.getLineReader();
        PromptResultItemIF resultItem = null;
        boolean validated = false;
        EnumSet<FieldType> typesNeedingStrValidation = EnumSet.of(
                FieldType.TEXT, FieldType.NUMBER, FieldType.CPF, FieldType.DATE, FieldType.DATETIME
        );

        while (!validated) {
            ConsolePrompt consolePrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
            PromptBuilder builder = consolePrompt.getPromptBuilder();
            PromptableElementIF jlineElement = buildJLinePromptElement(field, builder);
            if (jlineElement == null) {
                throw new IOException("Failed to build JLine prompt: " + field.getName());
            }

            List<AttributedString> currentHeader = new ArrayList<>(staticHeaderSupplier.get());
            currentHeader.addAll(dynamicErrorMessages);

            Map<String, PromptResultItemIF> resultMap = consolePrompt.prompt(currentHeader, List.of(jlineElement));
            dynamicErrorMessages.clear();

            if (resultMap == null || resultMap.isEmpty()) {
                throw new UserInterruptException("User interrupted (JLine): " + field.getName());
            }
            resultItem = resultMap.get(field.getName());
            if (resultItem == null) {
                throw new IOException("JLine prompt returned no result: " + field.getName());
            }

            if (!typesNeedingStrValidation.contains(field.getFieldType())) {
                validated = true;
            } else {
                String strValue = extractStringFromJLineResult(resultItem);

                if (field.getValidator().test(strValue)) {
                    validated = true;
                } else {
                    try {
                        field.getParser().apply(strValue);
                    } catch (Exception e) {
                        String error = "Invalid input for '" + field.getLabel() + "': " + e.getMessage();

                        dynamicErrorMessages.add(new AttributedString(error,
                                AttributedStyle.DEFAULT.foreground(AttributedStyle.RED)));
                    }
                }
            }
        }
        return resultItem;
    }

    private PromptableElementIF buildJLinePromptElement(FormField<?> field, PromptBuilder promptBuilder) {
        String defaultValueStr = formatDefaultValueForDisplay(field, true);
        String message = field.getLabel();

        switch (field.getFieldType()) {
            case TEXT, NUMBER ->
                    promptBuilder.createInputPrompt().name(field.getName()).message(message + ":")
                            .defaultValue(defaultValueStr).addPrompt();
            case CPF ->
                    promptBuilder.createInputPrompt().name(field.getName())
                            .message(message + " (format: 000.000.000-00):")
                            .defaultValue(defaultValueStr).addPrompt();
            case DATE ->
                    promptBuilder.createInputPrompt().name(field.getName())
                            .message(message + " (format: " + GlobalScope.DATE_FORMAT_STRING + "):")
                            .defaultValue(defaultValueStr).addPrompt();
            case DATETIME ->
                    promptBuilder.createInputPrompt().name(field.getName())
                            .message(message + " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "):")
                            .defaultValue(defaultValueStr).addPrompt();
            case CHOICE -> {
                var listPrompt = promptBuilder.createListPrompt()
                        .name(field.getName()).message(field.getLabel() + ":");
                field.getOptions().forEach((value, display) -> listPrompt.newItem(value).text(display).add());
                listPrompt.addPrompt();
            }
            case BOOLEAN -> {
                ConfirmChoice.ConfirmationValue defaultConfirm =
                        Boolean.TRUE.toString().equalsIgnoreCase(defaultValueStr) ?
                                ConfirmChoice.ConfirmationValue.YES : ConfirmChoice.ConfirmationValue.NO;
                promptBuilder.createConfirmPromp().name(field.getName()).message(field.getLabel())
                        .defaultValue(defaultConfirm).addPrompt();
            }
        }
        List<PromptableElementIF> elements = promptBuilder.build();
        return (elements != null && !elements.isEmpty()) ? elements.getFirst() : null;
    }

    private Map<String, String> gatherStdoutInput(InteractionProvider provider,
                                                  List<FormField<?>> initialFields) {
        Map<String, String> results = new HashMap<>();
        Deque<FormField<?>> fieldsToProcess = new LinkedList<>(initialFields);
        Set<String> processedFieldNames = new HashSet<>();

        while (!fieldsToProcess.isEmpty()) {
            FormField<?> field = fieldsToProcess.pollFirst();
            if (processedFieldNames.contains(field.getName())) continue;

            String value = promptAndValidateStdoutField(field, provider);
            results.put(field.getName(), value);
            processedFieldNames.add(field.getName());

            Map<String, List<FormField<?>>> conditionalChildrenMap = field.getConditionalChildren();
            if (conditionalChildrenMap == null || conditionalChildrenMap.isEmpty()) continue;

            List<FormField<?>> children = conditionalChildrenMap.get(value);
            addConditionalChildrenToQueue(children, fieldsToProcess, processedFieldNames);
        }
        return results;
    }

    private String promptAndValidateStdoutField(FormField<?> field, InteractionProvider provider) {
        PrintWriter writer = provider.getWriter();
        String inputValue = null;
        boolean validInput = false;

        while (!validInput) {
            writer.print(field.getLabel());

            String defaultValueDisplay = formatDefaultValueForDisplay(field, false);

            if (defaultValueDisplay != null && !defaultValueDisplay.isEmpty()) {
                writer.print(" (default: " + defaultValueDisplay + ")");
            }

            writer.print(getStdoutPromptSuffix(field, writer));
            writer.flush();

            String rawLine = provider.readLine("").trim();

            if (rawLine.isEmpty() && field.getDefaultValue() != null) {
                inputValue = getDefaultValueAsString(field);
                validInput = field.getValidator().test(inputValue);

                if (!validInput) {
                    writer.println("Warning: Default value (" + inputValue + ") for '" +
                            field.getLabel() + "' is invalid, but used anyway.");
                    validInput = true;
                }
            } else {
                String processed = processStdoutRawInput(field, rawLine, writer);

                if (processed == null) continue;

                inputValue = processed;
                validInput = field.getValidator().test(inputValue);

                if (!validInput) {
                    writer.println("Invalid input for '" + field.getLabel() + "'. Please try again.");
                }
            }
        }
        return inputValue;
    }

    private String getStdoutPromptSuffix(FormField<?> field, PrintWriter writer) {
        return switch (field.getFieldType()) {
            case CHOICE -> {
                writer.println();

                List<String> keys = new ArrayList<>(field.getOptions().keySet());

                for (int i = 0; i < keys.size(); i++) {
                    writer.printf("  %d. %s%n", i + 1, field.getOptions().get(keys.get(i)));
                }

                yield "Enter choice (number): ";
            }
            case BOOLEAN -> " (yes/no): ";
            case DATE -> " (format: " + GlobalScope.DATE_FORMAT_STRING + "): ";
            case DATETIME -> " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "): ";
            case CPF -> " (format: 000.000.000-00): ";
            default -> ": ";
        };
    }

    private String processStdoutRawInput(FormField<?> field, String rawLine, PrintWriter writer) {
        return switch (field.getFieldType()) {
            case CHOICE -> {
                try {
                    int choiceIdx = Integer.parseInt(rawLine) - 1;
                    List<String> keys = new ArrayList<>(field.getOptions().keySet());
                    if (choiceIdx >= 0 && choiceIdx < keys.size()) yield keys.get(choiceIdx);

                    writer.println("Invalid choice number. Please try again.");
                    yield null;
                } catch (NumberFormatException e) {
                    writer.println("Please enter a number for your choice. Please try again.");
                    yield null;
                }
            }
            case BOOLEAN -> {
                if ("yes".equalsIgnoreCase(rawLine) || "y".equalsIgnoreCase(rawLine)) yield Boolean.TRUE.toString();
                if ("no".equalsIgnoreCase(rawLine) || "n".equalsIgnoreCase(rawLine)) yield Boolean.FALSE.toString();
                yield rawLine;
            }
            default -> rawLine;
        };
    }

    private String getDefaultValueAsString(FormField<?> field) {
        Object defVal = field.getDefaultValue();
        if (defVal == null) return null;

        return switch (defVal) {
            case Enum<?> enumVal when field.getFieldType() == FieldType.CHOICE -> enumVal.name();
            case LocalDate ld when field.getFieldType() == FieldType.DATE -> GlobalScope.DATE_FORMAT.format(ld);
            case LocalDateTime ldt when field.getFieldType() == FieldType.DATETIME ->
                    GlobalScope.DATE_TIME_FORMAT.format(ldt);
            default -> defVal.toString();
        };
    }

    private String formatDefaultValueForDisplay(FormField<?> field, boolean isJLineContext) {
        Object defVal = field.getDefaultValue();

        if (defVal == null) return null;

        return switch (defVal) {
            case LocalDate ld -> GlobalScope.DATE_FORMAT.format(ld);
            case LocalDateTime ldt -> GlobalScope.DATE_TIME_FORMAT.format(ldt);
            case Enum<?> enumVal when field.getFieldType() == FieldType.CHOICE ->
                    isJLineContext ? enumVal.name() : field.getOptions().getOrDefault(enumVal.name(), enumVal.toString());
            case Boolean bVal when field.getFieldType() == FieldType.BOOLEAN -> {
                if (isJLineContext) yield bVal.toString();

                Map<String, String> options = field.getOptions();
                if (options != null) yield options.getOrDefault(bVal.toString(), bVal ? "yes" : "no");
                yield bVal ? "yes" : "no";
            }
            default -> defVal.toString();
        };
    }

    private void addConditionalChildrenToQueue(List<FormField<?>> children,
                                               Deque<FormField<?>> fieldsToProcess,
                                               Set<String> processedFieldNames) {
        if (children == null || children.isEmpty()) return;

        for (int i = children.size() - 1; i >= 0; i--) {
            FormField<?> child = children.get(i);

            if (!processedFieldNames.contains(child.getName()) &&
                    !fieldsToProcess.contains(child)) {
                fieldsToProcess.addFirst(child);
            }
        }
    }
}