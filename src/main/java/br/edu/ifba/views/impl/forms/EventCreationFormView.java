package br.edu.ifba.views.impl.forms;

import br.edu.ifba.applications.Application;
import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.enums.ConfirmationValue;
import br.edu.ifba.entities.enums.EventModality;
import br.edu.ifba.entities.enums.EventType;
import br.edu.ifba.repositories.impl.EventRepository;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.pages.types.FormPage;
import br.edu.ifba.ui.providers.StdoutInteractionProvider;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import br.edu.ifba.views.impl.MainView;
import org.jline.consoleui.elements.ConfirmChoice;
import org.jline.consoleui.elements.PromptableElementIF;
import org.jline.consoleui.prompt.ConfirmResult;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.consoleui.prompt.InputResult;
import org.jline.consoleui.prompt.ListResult;
import org.jline.consoleui.prompt.PromptResultItemIF;
import org.jline.consoleui.prompt.builder.ListPromptBuilder;
import org.jline.consoleui.prompt.builder.PromptBuilder;
import org.jline.reader.impl.completer.StringsCompleter; // For input autocompletion

import java.io.PrintWriter;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class EventCreationFormView extends FormPage implements View {
    public static final String NAME = "NEW_EVENT_FORM_VIEW";

    public EventCreationFormView() {
        super("Create New Event"); // Page title
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    protected List<PromptableElementIF> getJLineFormElements(ConsolePrompt prompt) {
        PromptBuilder promptBuilder = prompt.getPromptBuilder();

        // Event Name
        promptBuilder.createInputPrompt()
                .name("eventName")
                .message("Enter event name:")
                .defaultValue("Course about Programming")
                .addPrompt();

        // Event Date
        promptBuilder.createInputPrompt()
                .name("date")
                .message("Enter event date (e.g., 2025-06-15):")
                .addPrompt();

        // Event Place
        promptBuilder.createInputPrompt()
                .name("place")
                .message("Enter event place:")
                .addPrompt();

        // Event Type
        ListPromptBuilder eventType = promptBuilder.createListPrompt()
                .name("eventType")
                .message("Select event type:");

        for (EventType type : EventType.values()) {
            eventType.newItem(type.name()).text(type.getLabel()).add();
        }
        eventType.addPrompt();

        // Event Modality
        ListPromptBuilder modality = promptBuilder.createListPrompt()
                .name("modality")
                .message("Select event modality:");

        for (EventModality modalityType : EventModality.values()) {
            modality.newItem(modalityType.name()).text(modalityType.getLabel()).add();
        }
        modality.addPrompt();

        // Capacity
        promptBuilder.createInputPrompt()
                .name("capacity")
                .message("Enter event capacity (number):")
                .addPrompt();

        // Description
        promptBuilder.createInputPrompt()
                .name("description")
                .message("Enter event description:")
                .addPrompt();

        // Confirmation
        promptBuilder.createConfirmPromp()
                .name("confirmation")
                .message("Are you sure you want to create this event?")
                .defaultValue(ConfirmChoice.ConfirmationValue.YES)
                .addPrompt();

        return promptBuilder.build();
    }

    @Override
    protected void gatherStdoutInput(InteractionProvider provider, Map<String, String> resultsContainer) {
        PrintWriter writer = provider.getWriter();
        writer.println("--- Create New Event (STDOUT Mode) ---");

        String eventName = provider.readLine("Enter event name (default: My Awesome Event): ");
        resultsContainer.put("eventName", eventName.isEmpty() ? "My Awesome Event" : eventName);

        String date = provider.readLine("Enter event date (e.g., 2025-06-15): ");
        resultsContainer.put("date", date);

        String place = provider.readLine("Enter event place: ");
        resultsContainer.put("place", place);

        // Event Type
        writer.println("Select event type:");
        for (EventType type : EventType.values()) {
            writer.printf("%d. %s%n", type.ordinal() + 1, type.getLabel());
        }

        String typeChoice = provider.readLine("Enter choice (1-" + EventType.values().length + "): ");
        int typeIndex = typeChoice.isEmpty() ? 0 : Integer.parseInt(typeChoice) - 1;
        String eventType = EventType.values()[Math.max(0, Math.min(typeIndex, EventType.values().length - 1))].name();
        resultsContainer.put("eventType", eventType);

        // Modality
        writer.println("Select event modality:");
        for (EventModality modalityType : EventModality.values()) {
            writer.printf("%d. %s%n", modalityType.ordinal() + 1, modalityType.getLabel());
        }

        String modalityChoice = provider.readLine("Enter choice (1-" + EventModality.values().length + "): ");

        int modalityIndex = modalityChoice.isEmpty() ? 0 : Integer.parseInt(modalityChoice) - 1;
        String modality = EventModality.values()[Math.max(0, Math.min(modalityIndex, EventModality.values().length - 1))].name();

        resultsContainer.put("modality", modality);

        String capacity = provider.readLine("Enter event capacity (number): ");
        resultsContainer.put("capacity", capacity);

        String description = provider.readLine("Enter event description: ");
        resultsContainer.put("description", description);

        // Summary
        writer.println("\n--- Event Creation Summary ---");
        resultsContainer.forEach((k, v) -> writer.println(k + ": " + v));
        writer.println("----------------------------");

        // Confirmation
        boolean validChoice = false;
        while (!validChoice) {
            try {
                String confirmation = provider.readLine("Are you sure you want to create this event? (yes/no): ");
                ConfirmationValue confirmValue = ConfirmationValue.fromString(confirmation);

                if (confirmValue == ConfirmationValue.YES) {
                    resultsContainer.put("confirmation", "YES");
                    validChoice = true;
                } else if (confirmValue == ConfirmationValue.NO) {
                    resultsContainer.put("confirmation", "NO");
                    writer.println("Event creation cancelled.");
                    validChoice = true;
                } else {
                    throw new IllegalArgumentException();
                }
            } catch (Exception e) {
                writer.println("Invalid input. Please enter 'yes' or 'no'.");
            }
        }
    }

    @Override
    protected void onSubmit(InteractionProvider provider, Map<String, ?> formResults) {
        PrintWriter writer = provider.getWriter();

        writer.println("\n--- Event Creation Summary ---");

        if (formResults == null || formResults.isEmpty()) {
            writer.println("Form was cancelled or no data was entered.");
        } else {
            if (provider.getNativeProvider() instanceof StdoutInteractionProvider) {
                @SuppressWarnings("unchecked") Map<String, String> stdoutResults = (Map<String, String>) formResults;
                writer.println("Event Name: " + stdoutResults.get("eventName"));
                writer.println("Event Type: " + stdoutResults.get("eventType"));
                writer.println("Event Modality: " + stdoutResults.get("modality"));
            } else {
                @SuppressWarnings("unchecked") Map<String, PromptResultItemIF> jlineResults = (Map<String, PromptResultItemIF>) formResults;

                InputResult nameResult = (InputResult) jlineResults.get("eventName");
                ListResult typeResult = (ListResult) jlineResults.get("eventType");
                ListResult onlineResult = (ListResult) jlineResults.get("modality");

                writer.println("Event Name: " + (nameResult != null ? nameResult.getDisplayResult() : "N/A"));
                writer.println("Event Type: " + (typeResult != null ? typeResult.getDisplayResult() : "N/A"));
                writer.println("Event Modality: " + (onlineResult != null ? onlineResult.getDisplayResult() : "N/A"));
            }

            writer.println("----------------------------");
            writer.println("Event data captured (not saved in this demo).");

            /*
            try {

                        Event
                        EventRepository.INSTANCE.save()
                        writer.println("Event created successfully!");
                    } catch (Exception e) {
                        writer.println("Error creating event: " + e.getMessage());
                        Event.getSequentialIdProvider().rollback();
                    }
             */

        }

        writer.println("\nPress Enter to return to the main menu...");
        provider.readLine("");

        ViewRepository.INSTANCE.getById(MainView.NAME).ifPresent(Application::handleContextSwitch);
    }



    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new EventCreationFormView());
    }
}