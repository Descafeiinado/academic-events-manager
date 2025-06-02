package br.edu.ifba.applications;

import br.edu.ifba.AppConfig;
import br.edu.ifba.repositories.impl.EventRepository;
import br.edu.ifba.repositories.impl.PersonRepository;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.providers.JLineInteractionProvider;
import br.edu.ifba.ui.providers.StdoutInteractionProvider;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;

import java.io.IOException;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;

public class Application {

    private static Terminal terminalInstance;
    private static InteractionProvider currentInteractionProvider;
    private static final ScheduledExecutorService executorService = Executors.newScheduledThreadPool(
            Runtime.getRuntime().availableProcessors() + 1
    );

    public static void handleContextSwitch(String viewId) {
        View view = ViewRepository.INSTANCE.getById(viewId)
                .orElseThrow(() -> new IllegalStateException("View with ID '" + viewId + "' not found. Ensure it's initialized."));

        handleContextSwitch(view);
    }

    public static void handleContextSwitch(View view) {
        if (currentInteractionProvider == null) {
            System.err.println("Critical Error: InteractionProvider not initialized before context switch!");
            runLegacy();

            if (currentInteractionProvider == null) System.exit(1);
        }

        AppConfig.CURRENT_VIEW = view;

        view.display(currentInteractionProvider);
    }

    public static void finishGracefully() {
        PersonRepository.INSTANCE.persist();
        EventRepository.INSTANCE.persist();
    }

    public static void run() throws IOException {
        try {
            terminalInstance = TerminalBuilder.builder().system(true).nativeSignals(true).build();

            terminalInstance.handle(Terminal.Signal.INT, signal -> {
                System.out.println("\nCtrl+C detected. Exiting application in 1 second...");

                AppConfig.EXITED_ON_PURPOSE = true;

                finishGracefully();

                try {
                    executorService.schedule(() -> {
                        System.exit(0);
                    }, 1000, java.util.concurrent.TimeUnit.MILLISECONDS);

                    if (terminalInstance != null) terminalInstance.close();
                } catch (IOException ignored) {
                }
            });

            if (terminalInstance.getType().equals(Terminal.TYPE_DUMB) || terminalInstance.getType().equals(Terminal.TYPE_DUMB_COLOR)) {
                throw new IllegalStateException("Dumb terminal detected. Current terminal: " + terminalInstance.getType());
            }

            LineReader lineReader = LineReaderBuilder.builder()
                    .terminal(terminalInstance)
                    .build();

            currentInteractionProvider = new JLineInteractionProvider(terminalInstance, lineReader);

            currentInteractionProvider.getWriter().println("--- JLine Mode Active ---");
            currentInteractionProvider.getWriter().flush();

            View initialView = ViewRepository.INSTANCE.getById("MAIN").orElseThrow(() -> new IllegalStateException("Main view (MAIN) not found. Ensure it's initialized."));

            handleContextSwitch(initialView);

        } catch (UserInterruptException e) {
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
            View initialView = ViewRepository.INSTANCE.getById("MAIN").orElseThrow(() -> new IllegalStateException("Main view (MAIN) not found for legacy mode."));
            handleContextSwitch(initialView);
        } catch (Throwable t) {
            System.err.println("Critical error during legacy mode execution: " + t.getMessage());
            t.printStackTrace(System.err);
        }
    }
}