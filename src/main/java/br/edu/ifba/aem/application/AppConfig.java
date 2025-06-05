package br.edu.ifba.aem.application;

import br.edu.ifba.aem.ui.views.MainView;
import br.edu.ifba.aem.ui.views.View;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.utils.OSUtils;

public class AppConfig {

  public static boolean EXITED_ON_PURPOSE = false;
  public static boolean DEBUG_MODE = true;

  public static ConsolePrompt.UiConfig UI_CONFIG = generateUiConfig();

  public static View CURRENT_VIEW = new MainView();

  private static ConsolePrompt.UiConfig generateUiConfig() {
    ConsolePrompt.UiConfig config;

    if (OSUtils.IS_WINDOWS) {
      config = new ConsolePrompt.UiConfig(">", "( )", "(x)", "( )");
    } else {
      config = new ConsolePrompt.UiConfig("\u276F", "\u25EF ", "\u25C9 ", "\u25EF ");
    }

    config.setCancellableFirstPrompt(true);

    return config;
  }
}