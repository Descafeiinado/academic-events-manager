package br.edu.ifba.aem.ui.pages.types;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.pages.Page;
import br.edu.ifba.aem.ui.providers.JLineInteractionProvider;
import br.edu.ifba.aem.ui.providers.StdoutInteractionProvider;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.LinkedHashMap;
import java.util.Map;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.consoleui.prompt.ListResult;
import org.jline.consoleui.prompt.PromptResultItemIF;
import org.jline.consoleui.prompt.builder.ListPromptBuilder;
import org.jline.consoleui.prompt.builder.PromptBuilder;
import org.jline.reader.LineReader;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;

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

      EventRepository.INSTANCE.persist();

      System.exit(0);
    };

    String exitText = "Exit";
    navigationActions.put(exitText, exitAction);

    String exitKey = "0";

    stdoutOptionTextToKey.put(exitKey + ". " + exitText, exitKey);
    stdoutKeyToAction.put(exitKey, exitAction);
  }

  protected void addBackOption(InteractionProvider provider, String previousViewName) {
    Runnable backAction = () -> {
      provider.clearScreen();

      provider.getWriter().println("Going back to the previous menu...");
      provider.getWriter().flush();

      Application.handleContextSwitch(previousViewName);
    };

    String backText = "Back";
    navigationActions.put(backText, backAction);

    String backKey = String.valueOf(stdoutKeyToAction.size() + 1);

    stdoutOptionTextToKey.put(backKey + ". " + backText, backKey);
    stdoutKeyToAction.put(backKey, backAction);
  }

  public abstract void configureNavigationOptions(InteractionProvider provider);

  @Override
  public void display(InteractionProvider provider) {
    navigationActions.clear();
    stdoutOptionTextToKey.clear();
    stdoutKeyToAction.clear();

    configureNavigationOptions(provider);

    provider.clearScreen();

    renderTitle(provider);
    renderComponents(provider);

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

      if (listResult == null || listResult.getSelectedId() == null) {
        writer.println("Navigation cancelled.");
        writer.flush();
        return;
      }

      String selectedOption = listResult.getSelectedId();
      Runnable action = navigationActions.get(selectedOption);

      if (action != null) {
        action.run();
      } else {
        writer.println("Invalid selection processing. Action not found for: " + selectedOption);
      }
    } catch (UserInterruptException exception) {
      writer.println("\nNavigation cancelled by user (Ctrl+C).");
    } catch (IOException exception) {
      writer.println("Error displaying JLine navigation: " + exception.getMessage());
    }
  }

  private void displayStdoutNavigation(StdoutInteractionProvider sp,
      InteractionProvider baseProvider) {
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

      if (selectedAction == null) {
        writer.println("Invalid choice. Please try again.");
      }
    }
    selectedAction.run();
  }

}
