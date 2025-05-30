package br.edu.ifba.ui.pages.types;

import br.edu.ifba.AppConfig;
import br.edu.ifba.applications.Application;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.text.TextComponent;
import br.edu.ifba.ui.pages.Page;
import br.edu.ifba.ui.providers.JLineInteractionProvider;
import br.edu.ifba.ui.providers.StdoutInteractionProvider;
import br.edu.ifba.views.ViewRepository;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.consoleui.prompt.ListResult;
import org.jline.consoleui.prompt.PromptResultItemIF;
import org.jline.consoleui.prompt.builder.ListPromptBuilder;
import org.jline.consoleui.prompt.builder.PromptBuilder;
import org.jline.reader.LineReader;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.utils.AttributedString;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public abstract class NavigationPage extends Page {
    // Option display text -> Action to perform
    protected final LinkedHashMap<String, Runnable> navigationActions = new LinkedHashMap<>();

    // For STDOUT: Display text (including numeric key) -> Numeric key
    protected final LinkedHashMap<String, String> stdoutOptionTextToKey = new LinkedHashMap<>();
    // For STDOUT: Numeric key -> Action
    protected final LinkedHashMap<String, Runnable> stdoutKeyToAction = new LinkedHashMap<>();

    public NavigationPage(String title) {
        super(title);
    }

    protected void addNavigationOption(String displayText, String targetViewName) {
        Runnable action = () -> ViewRepository.INSTANCE.getById(targetViewName)
                .ifPresentOrElse(
                        Application::handleContextSwitch,
                        () -> System.err.println("Error: View '" + targetViewName + "' not found.")
                );
        addCustomAction(displayText, action);
    }

    protected void addCustomAction(String displayText, Runnable action) {
        navigationActions.put(displayText, action);

        // For STDOUT mode
        String key = String.valueOf(stdoutKeyToAction.size() + 1);

        stdoutOptionTextToKey.put(key + ". " + displayText, key);
        stdoutKeyToAction.put(key, action);
    }

    protected void addExitOption(InteractionProvider provider) {
        Runnable exitAction = () -> {
            provider.clearScreen();
            provider.getWriter().println("Exiting application...");
            provider.getWriter().flush();
            if (provider.getNativeProvider() instanceof JLineInteractionProvider) {
                try {
                    ((JLineInteractionProvider) provider.getNativeProvider()).getTerminal().close();
                } catch (IOException ignored) {
                }
            }
            System.exit(0);
        };

        String exitText = "Exit";
        navigationActions.put(exitText, exitAction);

        String exitKey = "0";

        stdoutOptionTextToKey.put(exitKey + ". " + exitText, exitKey);
        stdoutKeyToAction.put(exitKey, exitAction);
    }


    // Subclasses must call the addNavigationOption/addCustomAction methods within this.
    public abstract void configureNavigationOptions(InteractionProvider provider);

    @Override
    public void display(InteractionProvider provider) {
        // Clear previous options and configure anew each time display is called
        navigationActions.clear();
        stdoutOptionTextToKey.clear();
        stdoutKeyToAction.clear();

        configureNavigationOptions(provider); // Let subclass define options

        provider.clearScreen();

        renderTitle(provider);
        renderComponents(provider); // Render any static text

        if (navigationActions.isEmpty() && stdoutKeyToAction.isEmpty()) {
            provider.getWriter().println("No navigation options configured.");
            return;
        }

        if (provider instanceof JLineInteractionProvider) {
            displayJLineNavigation((JLineInteractionProvider) provider.getNativeProvider());
        } else if (provider instanceof StdoutInteractionProvider) {
            displayStdoutNavigation((StdoutInteractionProvider) provider.getNativeProvider(), provider);
        } else {
            provider.getWriter().println("Unsupported interaction provider for navigation.");
        }
    }

    private void displayJLineNavigation(JLineInteractionProvider jlp) {
        Terminal terminal = jlp.getTerminal();
        LineReader lineReader = jlp.getLineReader();
        PrintWriter writer = jlp.getWriter();

        if (navigationActions.isEmpty()) {
            writer.println("No JLine navigation options available.");
            return;
        }

        try {
            ConsolePrompt prompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
            PromptBuilder promptBuilder = prompt.getPromptBuilder();
            ListPromptBuilder listPrompt = promptBuilder.createListPrompt()
                    .name("navigationChoice")
                    .message("Please select an option:");

            for (String optionText : navigationActions.keySet()) {
                listPrompt.newItem(optionText).text(optionText).add();
            }

            Map<String, PromptResultItemIF> result = prompt.prompt(listPrompt.addPrompt().build());
            ListResult listResult = (ListResult) result.get("navigationChoice");

            if (listResult == null) {
                writer.println("Navigation cancelled.");
                return;
            }

            String selectedOption = listResult.getSelectedId();
            Runnable action = navigationActions.get(selectedOption);

            if (action != null)
                action.run();
            else writer.println("Invalid selection processing. Action not found for: " + selectedOption);
        } catch (UserInterruptException e) {
            writer.println("\nNavigation cancelled by user (Ctrl+C).");
        } catch (IOException e) {
            writer.println("Error displaying JLine navigation: " + e.getMessage());
        }
    }

    private void displayStdoutNavigation(StdoutInteractionProvider sp, InteractionProvider baseProvider) {
        PrintWriter writer = sp.getWriter();

        if (stdoutOptionTextToKey.isEmpty()) {
            writer.println("No STDOUT navigation options available.");
            return;
        }

        writer.println("Please select an option:");

        for (String optionDisplayText : stdoutOptionTextToKey.keySet()) {
            writer.println(optionDisplayText);
        }

        Runnable selectedAction = null;

        while (selectedAction == null) {
            String choiceKey = baseProvider.readLine("Enter your choice: ").trim();

            selectedAction = stdoutKeyToAction.get(choiceKey);

            if (selectedAction == null)
                writer.println("Invalid choice. Please try again.");
        }
        selectedAction.run();
    }
}