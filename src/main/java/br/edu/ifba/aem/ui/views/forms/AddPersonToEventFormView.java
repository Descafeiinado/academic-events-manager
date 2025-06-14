package br.edu.ifba.aem.ui.views.forms;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.services.ParticipationService;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.EventField;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.components.PersonField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.utils.ConsoleColors;
import br.edu.ifba.aem.ui.views.PeopleManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class AddPersonToEventFormView extends FormPage implements View {

  public static final String NAME = "ADD_PERSON_TO_EVENT_FORM_VIEW";

  public AddPersonToEventFormView() {
    super("Adding Person to Event");
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new AddPersonToEventFormView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(PersonField.of("person", "Enter Person's CPF"));

    Map<String, List<FormField<?>>> modalityCondition = Map.of("HYBRID", List.of(
        FormField.choice("modality", "Select Participation Modality", EventModality.IN_PERSON,
            EventModality.class, EventModality::getLabel, List.of(EventModality.HYBRID))));

    fields.add(EventField.of("event", "Enter Event ID").withConditionalChildren(modalityCondition));

    fields.add(
        FormField.confirmation("confirm", "Are you sure you want to add this Person to the Event?",
            true));

    return fields;
  }

  @Override
  public void onInterruptScreen(InteractionProvider provider) {
  }

  @Override
  protected void onSubmit(InteractionProvider provider, Map<String, Object> results) {
    PrintWriter writer = provider.getWriter();
    writer.println("\n--- Form Submission Processing ---");

    boolean hasNoData = results == null || results.isEmpty();
    boolean isCreationConfirmed = !hasNoData && Boolean.TRUE.equals(results.get("confirmCreation"));

    if (!isCreationConfirmed) {
      if (hasNoData) {
        writer.println("Form was cancelled, an error occurred, or no data was entered.");
      } else {
        writer.println("Person subscription cancelled by user.");
      }

      promptReturnToLatestMenu(provider);
      return;
    }

    try {
      String personCpf = (String) results.get("person");
      Long eventId = (Long) results.get("event");
      EventModality modality = (EventModality) results.get("modality");

      Event event = EventRepository.INSTANCE.getById(eventId)
          .orElseThrow(() -> new IllegalArgumentException("Event not found for ID: " + eventId));

      EventModality participationModality;

      if (event.getModality() == EventModality.HYBRID) {
        if (modality == null) {
          throw new IllegalArgumentException(
              "Participation modality must be selected for hybrid events.");
        }

        participationModality = modality;
      } else {
        participationModality = event.getModality();
      }

      ParticipationService.INSTANCE.participate(personCpf, eventId, participationModality);

      writer.println(ConsoleColors.GREEN_BOLD + "Person added to the event successfully!"
          + ConsoleColors.RESET);
    } catch (Exception exception) {
      writer.println(
          ConsoleColors.RED_BACKGROUND + ConsoleColors.RED + "Error adding person to event:"
              + ConsoleColors.RESET + " " + exception.getMessage());

      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace(writer);
      }
    }

    promptReturnToLatestMenu(provider);
  }

  private void promptReturnToLatestMenu(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\nPress Enter to return to the People Management menu...");
    provider.readLine("");

    ViewRepository.INSTANCE.getById(PeopleManagementView.NAME)
        .ifPresent(Application::handleContextSwitch);
  }

}