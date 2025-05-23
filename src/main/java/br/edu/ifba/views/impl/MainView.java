package br.edu.ifba.views.impl;

import br.edu.ifba.AppConfig;
import br.edu.ifba.Main;
import br.edu.ifba.views.InteractiveView;
import br.edu.ifba.views.impl.event.CreateEventView;

public class MainView implements InteractiveView {

    /*
        O sistema deve permitir:
            1. Cadastro de eventos de diferentes tipos (palestras, cursos, workshops, feiras).
            2. Associação de participantes a eventos (com controle de vagas disponíveis).
            3. Geração de certificados (texto) com dados do evento e do participante.
            4. Participantes podem ser alunos, professores ou externos, com dados específicos para cada categoria. Os participantes dos cursos devem ser exclusivamente alunos (não podem ser professores ou externos).
            5. Suporte a eventos híbridos (presenciais ou online), com comportamentos diferentes para o processo de inscrição.
            6. Relatório de eventos por tipo e data.
     */

    @Override
    public void render() {
        System.out.println("Available options: ");
        System.out.println();
        System.out.println("1. Register event");
        System.out.println("2. Register person");
        System.out.println("3. Generate certificate");
        System.out.println("4. Generate report");
        System.out.println("q. Exit");
        System.out.println();
        System.out.print("Please select an option: ");
    }

    @Override
    public void handleInput(String input) {
        switch (input) {
            case "1" -> Main.handleContextSwitch(new CreateEventView(), true);
            default -> {
                    System.out.println("Invalid input. Please try again.");
            }
        }
    }
}
