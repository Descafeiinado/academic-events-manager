package br.edu.ifba.aem.bootstrap;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import br.edu.ifba.aem.ui.views.ViewRepository;

public class Main {
    /*
        O sistema deve permitir:
            1. Cadastro de eventos de diferentes tipos (palestras, cursos, workshops, feiras).
            2. Associação de participantes a eventos (com controle de vagas disponíveis).
            3. Geração de certificados (texto) com dados do evento e do participante.
            4. Participantes podem ser alunos, professores ou externos, com dados específicos para cada categoria. Os participantes dos cursos devem ser exclusivamente alunos (não podem ser professores ou externos).
            5. Suporte a eventos híbridos (presenciais ou online), com comportamentos diferentes para o processo de inscrição.
            6. Relatório de eventos por tipo e data.
     */

  public static void main(String[] args) {
    EventRepository.INSTANCE.load();
    PersonRepository.INSTANCE.load();

    ViewRepository.initialize();

    try {
      System.out.println("Attempting to start in JLine mode...");

      Application.run();
//            throw new IOException("Simulating JLine failure for testing fallback to legacy mode");
    } catch (NoClassDefFoundError | ExceptionInInitializerError error) {
      if (AppConfig.EXITED_ON_PURPOSE) {
        return;
      }

      System.err.println(
          "JLine mode failed to start (JLine library issue): " + error.getClass().getSimpleName()
              + " - " + error.getMessage());
      System.err.println("Please ensure JLine library is correctly configured.");
      System.err.println("Falling back to legacy (Scanner) mode.");

      runLegacy();
    } catch (Throwable throwable) {
      if (AppConfig.EXITED_ON_PURPOSE) {
        return;
      }

      System.err.println("An unexpected error occurred while trying to run in JLine mode: "
          + throwable.getMessage());
      if (AppConfig.DEBUG_MODE) {
        throwable.printStackTrace();
      }
      System.err.println("Falling back to legacy (Scanner) mode.");

      runLegacy();
    }

    Application.finishGracefully();
  }

  private static void runLegacy() {
    try {
      Application.runLegacy();
    } catch (Throwable throwable) {
      if (AppConfig.EXITED_ON_PURPOSE) {
        return;
      }

      System.err.println("Critical error in legacy (Scanner) mode. Application will exit.");
      if (AppConfig.DEBUG_MODE) {
        throwable.printStackTrace();
      }
    }
  }
}