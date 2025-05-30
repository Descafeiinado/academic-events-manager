package br.edu.ifba.ui.pages.types;

import br.edu.ifba.AppConfig;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.pages.Page;
import br.edu.ifba.ui.providers.JLineInteractionProvider;
import br.edu.ifba.ui.providers.StdoutInteractionProvider;
import org.jline.consoleui.elements.PromptableElementIF;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.consoleui.prompt.PromptResultItemIF;
import org.jline.reader.LineReader;
import org.jline.reader.UserInterruptException;
import org.jline.terminal.Terminal;
import org.jline.utils.AttributedString;

import java.io.IOException;
import java.util.*;

public abstract class FormPage extends Page {
    protected Map<String, PromptResultItemIF> jlineResults;
    protected Map<String, String> stdoutResults;

    public FormPage(String title) {
        super(title);
    }

    protected abstract List<PromptableElementIF> getJLineFormElements(ConsolePrompt prompt);

    protected abstract void gatherStdoutInput(InteractionProvider provider, Map<String, String> resultsContainer);

    protected abstract void onSubmit(InteractionProvider provider, Map<String, ?> results);

    protected List<AttributedString> getFormHeader() {
        return new ArrayList<>();
    }

    @Override
    public void display(InteractionProvider provider) {
        provider.clearScreen();
        renderTitle(provider);
        renderComponents(provider); // Render any preliminary static components

        if (provider instanceof JLineInteractionProvider) {
            JLineInteractionProvider jlp = (JLineInteractionProvider) provider.getNativeProvider();
            Terminal terminal = jlp.getTerminal();
            LineReader lineReader = jlp.getLineReader();

            try {
                ConsolePrompt consolePrompt = new ConsolePrompt(lineReader, terminal, AppConfig.UI_CONFIG);
                List<PromptableElementIF> formElements = getJLineFormElements(consolePrompt);

                if (formElements == null || formElements.isEmpty()) {
                    provider.getWriter().println("No form elements defined for this JLine page.");
                    onSubmit(provider, Collections.emptyMap()); // Call onSubmit with empty results
                    return;
                }
                // For dynamic prompts (like JLineTest), the prompt() call would be more complex.
                // This version assumes a single list of prompts.
                this.jlineResults = consolePrompt.prompt(getFormHeader(), formElements);
                onSubmit(provider, this.jlineResults);

            } catch (UserInterruptException e) {
                provider.getWriter().println("\nForm cancelled by user (Ctrl+C).");

                onSubmit(provider, Collections.emptyMap()); // Indicate cancellation via empty map
            } catch (IOException e) {
                provider.getWriter().println("Error displaying JLine form: " + e.getMessage());
                // Consider a fallback or just error out
            }
        } else if (provider instanceof StdoutInteractionProvider) {
            this.stdoutResults = new HashMap<>();
            gatherStdoutInput(provider, this.stdoutResults);
            onSubmit(provider, this.stdoutResults);
        } else {
            provider.getWriter().println("Unsupported interaction provider for forms.");
        }
    }
}