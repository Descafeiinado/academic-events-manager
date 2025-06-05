package br.edu.ifba.aem.application;

import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.providers.JLineInteractionProvider;
import br.edu.ifba.aem.ui.providers.StdoutInteractionProvider;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;

public class Application {

  private static final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(
      1);
  private static Terminal terminalInstance;
  private static InteractionProvider currentInteractionProvider;

  public static void handleContextSwitch(String viewId) {
    View view = ViewRepository.INSTANCE.getById(viewId)
        .orElseThrow(() -> new IllegalStateException(
            "View with ID '" + viewId + "' not found. Ensure it's initialized."));

    handleContextSwitch(view);
  }

  public static void handleContextSwitch(View view) {
    if (currentInteractionProvider == null) {
      System.err.println(
          "Critical Error: InteractionProvider not initialized before context switch!");
      runLegacy();

      if (currentInteractionProvider == null) {
        System.exit(1);
      }
    }

    if (currentInteractionProvider.getNativeProvider() instanceof JLineInteractionProvider jLineInteractionProvider) {
      jLineInteractionProvider.resetTerminalForDisplay();
    }

    AppConfig.CURRENT_VIEW = view;

    view.display(currentInteractionProvider);
  }

  public static void handleInterruption() {
    if (AppConfig.EXITED_ON_PURPOSE) {
      System.out.println(
          "Application already exited on purpose. Probably locked in a loop or waiting for input.");
      System.out.println("Exiting gracefully without further action.");

      finishGracefully();

      System.exit(1);

      return;
    }

//        View interruptedView = AppConfig.CURRENT_VIEW;
//
//        if (interruptedView instanceof FormPage formPage) {
//            terminalInstance.flush();
//
//            formPage.onInterruptScreen(currentInteractionProvider);
//            return;
//        }

    System.out.println("\nCtrl+C detected. Exiting application in 1 second...");

    AppConfig.EXITED_ON_PURPOSE = true;

    finishGracefully();

    try {
      executorService.schedule(() -> {
        System.exit(0);
      }, 1000, java.util.concurrent.TimeUnit.MILLISECONDS);

      if (terminalInstance != null) {
        terminalInstance.close();
      }
    } catch (IOException ignored) {
    }
  }

  public static void finishGracefully() {
    PersonRepository.INSTANCE.persist();
    EventRepository.INSTANCE.persist();
  }

  public static void run() throws IOException {
    try {
      terminalInstance = TerminalBuilder.builder().system(true).nativeSignals(true).build();

      terminalInstance.handle(Terminal.Signal.INT, signal -> Application.handleInterruption());

      if (terminalInstance.getType().equals(Terminal.TYPE_DUMB) || terminalInstance.getType()
          .equals(Terminal.TYPE_DUMB_COLOR)) {
        throw new IllegalStateException(
            "Dumb terminal detected. Current terminal: " + terminalInstance.getType());
      }

      currentInteractionProvider = new JLineInteractionProvider(terminalInstance);

      currentInteractionProvider.getWriter().println("--- JLine Mode Active ---");
      currentInteractionProvider.getWriter().flush();

      View initialView = ViewRepository.INSTANCE.getById("MAIN").orElseThrow(
          () -> new IllegalStateException("Main view (MAIN) not found. Ensure it's initialized."));

      handleContextSwitch(initialView);

    } catch (UserInterruptException exception) {
      AppConfig.EXITED_ON_PURPOSE = true;

      System.out.println("\nOperation interrupted by user. Exiting.");

      if (terminalInstance != null) {
        terminalInstance.close();
      }
    } finally {
      AppConfig.EXITED_ON_PURPOSE = true;

      if (terminalInstance != null) {
        try {
          terminalInstance.close();
        } catch (IOException ignored) {
        }
      }

      System.out.println("Application terminated gracefully.");
    }
  }

  public static void runLegacy() {
    currentInteractionProvider = new StdoutInteractionProvider();

    currentInteractionProvider.getWriter().println("--- STDOUT Fallback Mode Active ---");
    currentInteractionProvider.getWriter().flush();

    try {
      View initialView = ViewRepository.INSTANCE.getById("MAIN").orElseThrow(
          () -> new IllegalStateException("Main view (MAIN) not found for legacy mode."));
      handleContextSwitch(initialView);
    } catch (Throwable throwable) {
      System.err.println("Critical error during legacy mode execution: " + throwable.getMessage());
      if (AppConfig.DEBUG_MODE) {
        throwable.printStackTrace(System.err);
      }
    }
  }
}