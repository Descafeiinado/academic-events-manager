package br.edu.ifba;

import br.edu.ifba.views.View;
import br.edu.ifba.views.impl.MainView;
import org.jline.consoleui.prompt.ConsolePrompt;
import org.jline.utils.OSUtils;

public class AppConfig {
    public static View CURRENT_VIEW = new MainView();

    public static ConsolePrompt.UiConfig UI_CONFIG = generateUiConfig();

    private static ConsolePrompt.UiConfig generateUiConfig() {
        ConsolePrompt.UiConfig config;

        if (OSUtils.IS_WINDOWS)
            config = new ConsolePrompt.UiConfig(">", "( )", "(x)", "( )");
        else
            config = new ConsolePrompt.UiConfig("\u276F", "\u25EF ", "\u25C9 ", "\u25EF ");

        config.setCancellableFirstPrompt(true);

        return config;
    }
}