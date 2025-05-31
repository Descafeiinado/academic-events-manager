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

import java.io.IOException;
import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

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

        Map<String, Object> parsedResults = new HashMap<>();

        try {
            Map<String, ?> rawResults;
            boolean isJLineProvider = provider.getNativeProvider() instanceof JLineInteractionProvider;

            if (isJLineProvider) {
                rawResults = gatherJLineInput((JLineInteractionProvider) provider.getNativeProvider(), initialFields);
            } else if (provider.getNativeProvider() instanceof StdoutInteractionProvider) {
                rawResults = gatherStdoutInput(provider, initialFields);
            } else {
                provider.getWriter().println("Unsupported interaction provider for forms.");
                onSubmit(provider, Collections.emptyMap());
                return;
            }

            if (rawResults == null) {
                onSubmit(provider, Collections.emptyMap());
                return;
            }

            List<FormField<?>> allPromptedFields = getFormFieldsRecursively(initialFields, rawResults, isJLineProvider);

            for (FormField<?> field : allPromptedFields) {
                Object rawValue = rawResults.get(field.getName());
                if (rawValue == null) continue;

                String stringValue;
                if (isJLineProvider) {
                    PromptResultItemIF item = (PromptResultItemIF) rawValue;
                    stringValue = extractStringFromJLineResult(item, field);
                    if (field.getFieldType() == FieldType.BOOLEAN && item instanceof ConfirmResult) {
                        parsedResults.put(field.getName(), ((ConfirmResult) item).getConfirmed() == ConfirmChoice.ConfirmationValue.YES);
                        continue;
                    }
                } else {
                    stringValue = (String) rawValue;
                }

                if (stringValue != null) {
                    if (field.getParser() != null) {
                        try {
                            parsedResults.put(field.getName(), field.getParser().apply(stringValue));
                        } catch (Exception parseException) {
                            provider.getWriter().println("Error parsing value for field '" + field.getLabel() + "': \"" + stringValue + "\". Error: " + parseException.getMessage());
                        }
                    } else {
                        parsedResults.put(field.getName(), stringValue);
                    }
                }
            }
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

    private List<FormField<?>> getFormFieldsRecursively(List<FormField<?>> initialFields, Map<String, ?> rawResults, boolean isJLine) {
        List<FormField<?>> allEffectivelyPromptedFields = new ArrayList<>();
        Queue<FormField<?>> fieldsToConsider = new LinkedList<>(initialFields);
        Set<String> visitedFieldNames = new HashSet<>();

        while(!fieldsToConsider.isEmpty()) {
            FormField<?> currentField = fieldsToConsider.poll();
            if (visitedFieldNames.contains(currentField.getName())) continue;

            if (rawResults.containsKey(currentField.getName())) {
                allEffectivelyPromptedFields.add(currentField);
                visitedFieldNames.add(currentField.getName());

                if (currentField.getFieldType() == FieldType.CHOICE &&
                        currentField.getConditionalChildren() != null &&
                        !currentField.getConditionalChildren().isEmpty()) {
                    Object resultValue = rawResults.get(currentField.getName());
                    String selectedChoiceKey = isJLine ?
                            extractStringFromJLineResult((PromptResultItemIF) resultValue, currentField) :
                            (String) resultValue;

                    if (selectedChoiceKey != null) {
                        List<FormField<?>> children = currentField.getConditionalChildren().get(selectedChoiceKey);
                        if (children != null) {
                            for(FormField<?> child : children){
                                if(!visitedFieldNames.contains(child.getName())){
                                    fieldsToConsider.add(child);
                                }
                            }
                        }
                    }
                }
            }
        }
        return allEffectivelyPromptedFields;
    }

    private String extractStringFromJLineResult(PromptResultItemIF item, FormField<?> field) {
        if (item == null) return null;
        if (item instanceof InputResult) {
            return ((InputResult) item).getResult();
        } else if (item instanceof ListResult) {
            return ((ListResult) item).getSelectedId();
        } else if (item instanceof ConfirmResult) {
            ConfirmChoice.ConfirmationValue confirmed = ((ConfirmResult) item).getConfirmed();
            return (confirmed == ConfirmChoice.ConfirmationValue.YES) ? Boolean.TRUE.toString() : Boolean.FALSE.toString();
        }
        return item.getDisplayResult();
    }

    private Map<String, PromptResultItemIF> gatherJLineInput(JLineInteractionProvider jlp, List<FormField<?>> initialFields) throws IOException, UserInterruptException {
        Terminal terminal = jlp.getTerminal();
        LineReader lineReader = jlp.getLineReader();

        Map<String, PromptResultItemIF> allResults = new HashMap<>();
        Deque<FormField<?>> fieldsToProcess = new LinkedList<>(initialFields);
        Set<String> processedFieldNamesOutputInAllResults = new HashSet<>();

        while (!fieldsToProcess.isEmpty()) {
            FormField<?> field = fieldsToProcess.peekFirst();

            if (processedFieldNamesOutputInAllResults.contains(field.getName())) {
                fieldsToProcess.pollFirst();
                continue;
            }

            boolean currentFieldValidated = false;
            PromptResultItemIF resultItemForCurrentField = null;

            while(!currentFieldValidated) {
                ConsolePrompt consolePrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
                PromptBuilder singleElementPromptBuilder = consolePrompt.getPromptBuilder();
                PromptableElementIF jlineElement = null;

                String defaultValueStr = null;
                if (field.getDefaultValue() != null) {
                    Object defVal = field.getDefaultValue();
                    defaultValueStr = switch (defVal) {
                        case LocalDate localDate when field.getFieldType() == FieldType.DATE ->
                                GlobalScope.DATE_FORMAT.format(localDate);
                        case LocalDateTime localDateTime when field.getFieldType() == FieldType.DATETIME ->
                                GlobalScope.DATE_TIME_FORMAT.format(localDateTime);
                        case Enum anEnum when field.getFieldType() == FieldType.CHOICE -> anEnum.name();
                        default -> defVal.toString();
                    };
                }

                String message = field.getLabel();
                switch (field.getFieldType()) {
                    case TEXT: case NUMBER:
                        message += ":";
                        singleElementPromptBuilder.createInputPrompt()
                                .name(field.getName())
                                .message(message)
                                .defaultValue(defaultValueStr)
                                .addPrompt(); // Adds the configured prompt to the builder
                        break;
                    case CPF:
                        message += " (format: 000.000.000-00):";
                        singleElementPromptBuilder.createInputPrompt()
                                .name(field.getName())
                                .message(message)
                                .defaultValue(defaultValueStr)
                                .addPrompt();
                        break;
                    case DATE:
                        message += " (format: " + GlobalScope.DATE_FORMAT_STRING + "):";
                        singleElementPromptBuilder.createInputPrompt()
                                .name(field.getName())
                                .message(message)
                                .defaultValue(defaultValueStr)
                                .addPrompt();
                        break;
                    case DATETIME:
                        message += " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "):";
                        singleElementPromptBuilder.createInputPrompt()
                                .name(field.getName())
                                .message(message)
                                .defaultValue(defaultValueStr)
                                .addPrompt();
                        break;
                    case CHOICE:
                        var listPromptConfig = singleElementPromptBuilder.createListPrompt()
                                .name(field.getName())
                                .message(field.getLabel() + ":");
                        field.getOptions().forEach((value, displayText) -> listPromptConfig.newItem(value).text(displayText).add());

                        listPromptConfig.addPrompt();
                        break;
                    case BOOLEAN:
                        singleElementPromptBuilder.createConfirmPromp()
                                .name(field.getName())
                                .message(field.getLabel())
                                .defaultValue(Boolean.TRUE.equals(field.getDefaultValue()) ? ConfirmChoice.ConfirmationValue.YES : ConfirmChoice.ConfirmationValue.NO)
                                .addPrompt();
                        break;
                }

                List<PromptableElementIF> elements = singleElementPromptBuilder.build();

                if (elements == null || elements.isEmpty()) {
                    throw new IOException("Failed to build JLine prompt element for field: " + field.getName());
                }

                jlineElement = elements.get(0);

                Map<String, PromptResultItemIF> currentResultMap = consolePrompt.prompt(getFormHeader(), List.of(jlineElement));

                if (currentResultMap == null || currentResultMap.isEmpty()) {
                    throw new UserInterruptException("User interrupted during JLine prompt for field: " + field.getName());
                }

                resultItemForCurrentField = currentResultMap.get(field.getName());
                if (resultItemForCurrentField == null) {
                    throw new IOException("JLine prompt returned no result for field: " + field.getName());
                }

                if (field.getFieldType() == FieldType.TEXT || field.getFieldType() == FieldType.NUMBER ||
                        field.getFieldType() == FieldType.DATE || field.getFieldType() == FieldType.DATETIME) {
                    String stringValueToValidate = extractStringFromJLineResult(resultItemForCurrentField, field);
                    if (field.getValidator().test(stringValueToValidate)) {
                        currentFieldValidated = true;
                    } else {
                        terminal.writer().println("Invalid input for '" + field.getLabel() + "'. Please try again.");
                        terminal.writer().flush();
                    }
                } else {
                    currentFieldValidated = true;
                }
            }

            allResults.put(field.getName(), resultItemForCurrentField);
            processedFieldNamesOutputInAllResults.add(field.getName());
            fieldsToProcess.pollFirst();

            if (field.getConditionalChildren() != null && !field.getConditionalChildren().isEmpty()) {
                String selectedStringValue = extractStringFromJLineResult(resultItemForCurrentField, field);
                if (selectedStringValue != null) {
                    List<FormField<?>> children = field.getConditionalChildren().get(selectedStringValue);
                    if (children != null && !children.isEmpty()) {
                        for (int i = children.size() - 1; i >= 0; i--) {
                            if (!processedFieldNamesOutputInAllResults.contains(children.get(i).getName())) {
                                fieldsToProcess.addFirst(children.get(i));
                            }
                        }
                    }
                }
            }
        }
        return allResults;
    }

    private Map<String, String> gatherStdoutInput(InteractionProvider provider, List<FormField<?>> initialFields) {
        PrintWriter writer = provider.getWriter();
        Map<String, String> resultsContainer = new HashMap<>();
        Deque<FormField<?>> fieldsToProcess = new LinkedList<>(initialFields);
        Set<String> processedFieldNames = new HashSet<>();

        while (!fieldsToProcess.isEmpty()) {
            FormField<?> field = fieldsToProcess.pollFirst();

            if (processedFieldNames.contains(field.getName())) continue;

            boolean validInput = false;
            String selectedStringValue = null;

            while (!validInput) {
                writer.print(field.getLabel());
                writer.flush();

                String defaultValueDisplay = "";
                if (field.getDefaultValue() != null) {
                    Object defVal = field.getDefaultValue();
                    defaultValueDisplay = switch (defVal) {
                        case Enum anEnum when field.getFieldType() == FieldType.CHOICE ->
                                field.getOptions().getOrDefault(anEnum.name(), defVal.toString());
                        case Boolean b when field.getFieldType() == FieldType.BOOLEAN ->
                                field.getOptions().getOrDefault(defVal.toString(), defVal.toString());
                        case LocalDate localDate when field.getFieldType() == FieldType.DATE ->
                                GlobalScope.DATE_FORMAT.format(localDate);
                        case LocalDateTime localDateTime when field.getFieldType() == FieldType.DATETIME ->
                                GlobalScope.DATE_TIME_FORMAT.format(localDateTime);
                        default -> defVal.toString();
                    };
                    writer.print(" (default: " + defaultValueDisplay + ")");
                }

                String promptSuffix = "";
                if (field.getFieldType() == FieldType.CHOICE) {
                    writer.println();
                    List<String> optionKeys = new ArrayList<>(field.getOptions().keySet());
                    for (int i = 0; i < optionKeys.size(); i++) {
                        writer.printf("  %d. %s%n", i + 1, field.getOptions().get(optionKeys.get(i)));
                    }
                    promptSuffix = "Enter choice (number): ";
                } else if (field.getFieldType() == FieldType.BOOLEAN) {
                    promptSuffix = " (yes/no): ";
                } else if (field.getFieldType() == FieldType.DATE) {
                    promptSuffix = " (format: " + GlobalScope.DATE_FORMAT_STRING + "): ";
                } else if (field.getFieldType() == FieldType.DATETIME) {
                    promptSuffix = " (format: " + GlobalScope.DATE_TIME_FORMAT_STRING + "): ";
                }

                writer.print(promptSuffix.isEmpty() ? ": " : promptSuffix);
                writer.flush();

                String rawLine = provider.readLine("").trim();

                if (rawLine.isEmpty() && field.getDefaultValue() != null) {
                    Object defValObj = field.getDefaultValue();
                    if (field.getFieldType() == FieldType.CHOICE) {
                        selectedStringValue = defValObj.toString().toUpperCase();
                    } else if (field.getFieldType() == FieldType.BOOLEAN) {
                        selectedStringValue = defValObj.toString();
                    } else if (field.getFieldType() == FieldType.DATE) {
                        selectedStringValue = GlobalScope.DATE_FORMAT.format((LocalDate)defValObj);
                    } else if (field.getFieldType() == FieldType.DATETIME) {
                        selectedStringValue = GlobalScope.DATE_TIME_FORMAT.format((LocalDateTime)defValObj);
                    } else {
                        selectedStringValue = defValObj.toString();
                    }

                    validInput = field.getValidator().test(selectedStringValue);

                    if (!validInput) writer.println("Warning: Default value (" + selectedStringValue + ") failed validation. Using it anyway.");

                    validInput = true;
                } else {
                    if (field.getFieldType() == FieldType.CHOICE) {
                        try {
                            int choiceIndex = Integer.parseInt(rawLine) - 1;
                            List<String> optionKeys = new ArrayList<>(field.getOptions().keySet());
                            if (choiceIndex >= 0 && choiceIndex < optionKeys.size()) {
                                selectedStringValue = optionKeys.get(choiceIndex);
                            } else {
                                writer.println("Invalid choice number."); continue;
                            }
                        } catch (NumberFormatException e) {
                            writer.println("Please enter a number for your choice."); continue;
                        }
                    } else {
                        selectedStringValue = rawLine;
                    }
                    validInput = field.getValidator().test(selectedStringValue);
                    if (!validInput) writer.println("Invalid input. Please try again.");
                }
            }

            resultsContainer.put(field.getName(), selectedStringValue);
            processedFieldNames.add(field.getName());

            if (field.getConditionalChildren() != null && !field.getConditionalChildren().isEmpty()) {
                List<FormField<?>> children = field.getConditionalChildren().get(selectedStringValue);
                if (children != null && !children.isEmpty()) {
                    for (int i = children.size() - 1; i >= 0; i--) {
                        if (!processedFieldNames.contains(children.get(i).getName())) {
                            fieldsToProcess.addFirst(children.get(i));
                        }
                    }
                }
            }
        }
        return resultsContainer;
    }
}