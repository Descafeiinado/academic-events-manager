package br.edu.ifba.views.impl.event;

import br.edu.ifba.views.InteractiveView;

public class CreateEventView implements InteractiveView {

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
        System.out.println();
        System.out.println(" CREATING A NEW EVENT");
        System.out.println();
        System.out.println("Press q to Exit");
        System.out.println();

//        String type = Prompt.ask("Enter the type of event (lecture, course, workshop, fair): ", "^(lecture|course|workshop|fair)$");
//        String name = Prompt.ask("Enter the name of the event: ", "^[a-zA-Z0-9 ]+$");
//        String date = Prompt.ask("Enter the date of the event (YYYY-MM-DD): ", "^\\d{4}-\\d{2}-\\d{2}$");
//        String time = Prompt.ask("Enter the time of the event (HH:MM): ", "^\\d{2}:\\d{2}$");
//
//        String location = Prompt.ask("Enter the location of the event: ", "^[a-zA-Z0-9 ]+$");

        System.out.println();
        System.out.println("Event created successfully!");

//        System.out.println("Type: " + type + ", Name: " + name + ", Date: " + date + ", Time: " + time + ", Location: " + location);
    }

    @Override
    public void handleInput(String input) {
        System.out.println("Input passed = " + input);
    }
}
