package br.edu.ifba.ui.providers;

import br.edu.ifba.ui.common.InteractionProvider;
import org.jline.reader.LineReader;
import org.jline.terminal.Terminal;
import org.jline.utils.InfoCmp;

import java.io.PrintWriter;

public class JLineInteractionProvider implements InteractionProvider {
    private final Terminal terminal;
    private final LineReader lineReader;
    private final PrintWriter writer;

    public JLineInteractionProvider(Terminal terminal, LineReader lineReader) {
        this.terminal = terminal;
        this.lineReader = lineReader;
        this.writer = terminal.writer();
    }

    @Override
    public PrintWriter getWriter() {
        return writer;
    }

    @Override
    public String readLine(String prompt) {
        return lineReader.readLine(prompt);
    }

    @Override
    public void clearScreen() {
        terminal.puts(InfoCmp.Capability.clear_screen);
        terminal.flush();
    }

    @Override
    public Object getNativeProvider() {
        return this;
    }

    public Terminal getTerminal() {
        return terminal;
    }

    public LineReader getLineReader() {
        return lineReader;
    }
}