package br.edu.ifba;

import br.edu.ifba.utils.TerminalUtils;
import br.edu.ifba.views.View;
import br.edu.ifba.views.Interactive;
import br.edu.ifba.views.impl.MainView;

import java.util.List;
import java.util.Scanner;

public class Main {
    public static View PREVIOUS_VIEW = null;
    public static View CURRENT_VIEW = new MainView();

    public static List<String> VALID_EXIT_COMMANDS = List.of("exit", "sair", "q", "quit");
    public static List<String> VALID_BACK_COMMANDS = List.of("back", "voltar", "b", "v");

    /*
        O sistema deve permitir:
            1. Cadastro de eventos de diferentes tipos (palestras, cursos, workshops, feiras).
            2. Associação de participantes a eventos (com controle de vagas disponíveis).
            3. Geração de certificados (texto) com dados do evento e do participante.
            4. Participantes podem ser alunos, professores ou externos, com dados específicos para cada categoria. Os participantes dos cursos devem ser exclusivamente alunos (não podem ser professores ou externos).
            5. Suporte a eventos híbridos (presenciais ou online), com comportamentos diferentes para o processo de inscrição.
            6. Relatório de eventos por tipo e data.
     */

    public static void handleContextSwitch(View view, boolean history) {
        TerminalUtils.clearTerminal();

        PREVIOUS_VIEW = history ? CURRENT_VIEW : null;
        CURRENT_VIEW = view;
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        while (true) {
            CURRENT_VIEW.render();

            String input = scanner.nextLine();

            boolean hasPrevious = PREVIOUS_VIEW != null;

            if (!hasPrevious) { // No previous view to go back to
                boolean isExitCommand = VALID_EXIT_COMMANDS.contains(input.toLowerCase());

                if (isExitCommand) {
                    System.out.println("Exiting the application...");
                    break;
                }
            }

            if (input.isBlank()) {
                if (hasPrevious) {
                    handleContextSwitch(PREVIOUS_VIEW, false);
                } else { System.out.println("No previous view to go back to."); }
            }

            if (CURRENT_VIEW instanceof Interactive) {
                Interactive interactiveView = (Interactive) CURRENT_VIEW;

                interactiveView.handleInput(input);
            } else { System.out.println("Invalid input. Please try again."); }
        }
    }
}