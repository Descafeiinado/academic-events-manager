package br.edu.ifba.aem.ui.providers;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import java.io.PrintWriter;
import lombok.Getter;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.terminal.Terminal;
import org.jline.utils.InfoCmp;

public class JLineInteractionProvider implements InteractionProvider {

  @Getter
  private final Terminal terminal;
  private final PrintWriter writer;
  @Getter
  private LineReader lineReader;

  public JLineInteractionProvider(Terminal terminal) {
    this.terminal = terminal;
    this.writer = terminal.writer();

    initializeLineReader();
  }

  private void initializeLineReader() {
    try {
      this.lineReader = LineReaderBuilder.builder()
          .terminal(terminal)
          .build();
    } catch (Exception e) {
      System.err.println(
          "JLineInteractionProvider: Error initializing LineReader: " + e.getMessage());

      if (AppConfig.DEBUG_MODE) {
        e.printStackTrace(System.err);
      }
    }
  }

  public void resetTerminalForDisplay() {
    if (terminal != null) {
      initializeLineReader();

      try {
        clearScreen();

        terminal.flush();
      } catch (Exception exception) {
        System.err.println("JLineInteractionProvider: Error resetting terminal for display: "
            + exception.getMessage());

        if (AppConfig.DEBUG_MODE) {
          exception.printStackTrace(System.err);
        }
      }
    }
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
}