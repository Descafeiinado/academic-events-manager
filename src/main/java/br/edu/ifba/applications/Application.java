package br.edu.ifba.applications;

import br.edu.ifba.AppConfig;
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

public class Application {

    private static Terminal terminalInstance;
    private static InteractionProvider currentInteractionProvider;

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

    public static void run() throws IOException {
        try {
            terminalInstance = TerminalBuilder.builder().system(true).nativeSignals(true).build();

            // Graceful exit on Ctrl+C
            terminalInstance.handle(Terminal.Signal.INT, signal -> {
                System.out.println("\nCtrl+C detected. Exiting application...");
                try {
                    if (terminalInstance != null) {
                        terminalInstance.close();
                    }
                } catch (IOException ignored) {
                }
                System.exit(0);
            });

            if (terminalInstance.getType().equals(Terminal.TYPE_DUMB) || terminalInstance.getType().equals(Terminal.TYPE_DUMB_COLOR)) {
                throw new IllegalStateException("Dumb terminal detected. Current terminal: " + terminalInstance.getType());
            }

            // Consider adding history for LineReader
            // String historyFile = Paths.get(System.getProperty("user.home"), ".your_app_history").toString();
            LineReader lineReader = LineReaderBuilder.builder().terminal(terminalInstance)
                    // .variable(LineReader.HISTORY_FILE, historyFile)
                    .build();

            currentInteractionProvider = new JLineInteractionProvider(terminalInstance, lineReader);

            currentInteractionProvider.getWriter().println("--- JLine Mode Active ---");
            currentInteractionProvider.getWriter().flush();
            // Consider a brief pause or "Press Enter to continue" if needed

            View initialView = ViewRepository.INSTANCE.getById("MAIN").orElseThrow(() -> new IllegalStateException("Main view (MAIN) not found. Ensure it's initialized."));
            handleContextSwitch(initialView);

        } catch (UserInterruptException e) {
            // This can happen if Ctrl+C is pressed during LineReader operations outside ConsolePrompt
            System.out.println("\nOperation interrupted by user. Exiting.");
            if (terminalInstance != null) {
                terminalInstance.close();
            }
        } finally {
            if (terminalInstance != null) {
                try {
                    terminalInstance.close();
                } catch (IOException e) {
                    System.err.println("Error closing JLine terminal: " + e.getMessage());
                }
            }

            System.out.print("\033[H\033[2J");
            System.out.flush();

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