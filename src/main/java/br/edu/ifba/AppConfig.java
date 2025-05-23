package br.edu.ifba;

import br.edu.ifba.views.View;
import br.edu.ifba.views.impl.MainView;

import java.util.List;

public class AppConfig {
    public static View PREVIOUS_VIEW = null;
    public static View CURRENT_VIEW = new MainView();

    public static final List<String> VALID_EXIT_COMMANDS = List.of("exit", "sair", "q", "quit");
    public static final List<String> VALID_BACK_COMMANDS = List.of("back", "voltar", "b", "v");
}