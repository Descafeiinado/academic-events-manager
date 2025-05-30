package br.edu.ifba.views.impl.forms;

import br.edu.ifba.applications.Application;
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
import org.jline.consoleui.prompt.builder.PromptBuilder;
import org.jline.reader.impl.completer.StringsCompleter; // For input autocompletion

import java.io.PrintWriter;
import java.util.List;
import java.util.Map;

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

        promptBuilder.createInputPrompt()
                .name("eventName")
                .message("Enter event name:")
                .defaultValue("My Awesome Event")
                .addCompleter(new StringsCompleter("Conference", "Workshop", "Meetup", "Lecture"))
                .addPrompt();

        promptBuilder.createListPrompt()
                .name("eventType")
                .message("Select event type:")
                .newItem("CONFERENCE").text("Conference").add()
                .newItem("WORKSHOP").text("Workshop").add()
                .newItem("SEMINAR").text("Seminar").add()
                .newItem("LECTURE").text("Lecture").add()
                .addPrompt();

        promptBuilder.createConfirmPromp()
                .name("isOnline")
                .message("Is the event online?")
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

        writer.println("Select event type:");
        writer.println("1. Conference");
        writer.println("2. Workshop");
        writer.println("3. Seminar");
        writer.println("4. Lecture");
        String typeChoice = "";
        String eventType = "";
        boolean validChoice = false;
        while (!validChoice) {
            typeChoice = provider.readLine("Enter choice (1-4): ");
            switch (typeChoice) {
                case "1": eventType = "CONFERENCE"; validChoice = true; break;
                case "2": eventType = "WORKSHOP"; validChoice = true; break;
                case "3": eventType = "SEMINAR"; validChoice = true; break;
                case "4": eventType = "LECTURE"; validChoice = true; break;
                default: writer.println("Invalid choice. Please try again.");
            }
        }
        resultsContainer.put("eventType", eventType);

        String onlineChoice = "";
        while (!onlineChoice.equalsIgnoreCase("y") && !onlineChoice.equalsIgnoreCase("n")) {
            onlineChoice = provider.readLine("Is the event online? (y/n, default: y): ");
            if (onlineChoice.isEmpty()) onlineChoice = "y"; // Default to 'y' if empty
        }
        resultsContainer.put("isOnline", onlineChoice.equalsIgnoreCase("y") ? "YES" : "NO");
    }

    @Override
    protected void onSubmit(InteractionProvider provider, Map<String, ?> formResults) {
        PrintWriter writer = provider.getWriter();
        writer.println("\n--- Event Creation Summary ---");

        if (formResults == null || formResults.isEmpty()) {
            writer.println("Form was cancelled or no data was entered.");
        } else {
            if (provider.getNativeProvider() instanceof StdoutInteractionProvider) {
                // STDOUT results are Map<String, String>
                @SuppressWarnings("unchecked")
                Map<String, String> SResults = (Map<String, String>) formResults;
                writer.println("Event Name: " + SResults.get("eventName"));
                writer.println("Event Type: " + SResults.get("eventType"));
                writer.println("Is Online: " + SResults.get("isOnline"));
            } else {
                // JLine results are Map<String, PromptResultItemIF>
                @SuppressWarnings("unchecked")
                Map<String, PromptResultItemIF> jResults = (Map<String, PromptResultItemIF>) formResults;

                InputResult nameResult = (InputResult) jResults.get("eventName");
                ListResult typeResult = (ListResult) jResults.get("eventType");
                ConfirmResult onlineResult = (ConfirmResult) jResults.get("isOnline");

                writer.println("Event Name: " + (nameResult != null ? nameResult.getDisplayResult() : "N/A"));
                writer.println("Event Type: " + (typeResult != null ? typeResult.getSelectedId() : "N/A"));
                writer.println("Is Online: " + (onlineResult != null ? onlineResult.getConfirmed() : "N/A"));
            }

            writer.println("----------------------------");
            // Here you would typically save the event data or process it further.
            writer.println("Event data captured (not saved in this demo).");
        }

        writer.println("\nPress Enter to return to the main menu...");
        provider.readLine(""); // Wait for user acknowledgment

        // Navigate back to MainView
        ViewRepository.INSTANCE.getById(MainView.NAME)
                .ifPresent(Application::handleContextSwitch);
    }

    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new EventCreationFormView());
    }
}