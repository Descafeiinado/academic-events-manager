package br.edu.ifba.aem.ui.views.forms;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.events.Course;
import br.edu.ifba.aem.domain.entities.events.Fair;
import br.edu.ifba.aem.domain.entities.events.Lecture;
import br.edu.ifba.aem.domain.entities.events.Workshop;
import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.service.ParticipationService;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.EventField;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.components.PersonField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.views.MainView;
import br.edu.ifba.aem.ui.views.PeopleManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.PrintWriter;
import java.time.LocalDateTime;
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

    Map<String, List<FormField<?>>> modalityCondition = Map.of(
        "HYBRID", List.of(FormField.choice(
            "modality", "Select Participation Modality",
            EventModality.IN_PERSON, // default value
            EventModality.class,
            EventModality::getLabel,
            List.of(EventModality.HYBRID)
        ))
    );

    fields.add(EventField.of("event", "Enter Event ID")
        .withConditionalChildren(modalityCondition));

    fields.add(FormField.confirmation(
        "confirm", "Are you sure you want to add this Person to the Event?", true));

    return fields;
  }

  @Override
  public void onInterruptScreen(InteractionProvider provider) {
  }

  @Override
  protected void onSubmit(InteractionProvider provider, Map<String, Object> results) {
    PrintWriter writer = provider.getWriter();
    writer.println("\n--- Form Submission Processing ---");

    if (results == null || results.isEmpty() || !Boolean.TRUE.equals(results.get("confirm"))) {
      if (results != null && !results.isEmpty() && !Boolean.TRUE.equals(results.get("confirm"))) {
        writer.println("Event creation cancelled by user.");
      } else {
        writer.println("Form was cancelled, an error occurred, or no data was entered.");
      }
    } else {
      writer.println("Received data:");

      results.forEach((key, value) -> {
        if (!key.equals("confirm")) {
          writer.println(String.format("  %s: %s (Type: %s)",
              key,
              value,
              value != null ? value.getClass().getSimpleName() : "null"));
        }
      });

      try {
        String personCpf = (String) results.get("person");
        Long eventId = (Long) results.get("event");
        EventModality modality = (EventModality) results.get("modality");

        Event event = EventRepository.INSTANCE.getById(eventId)
            .orElseThrow(() -> new IllegalArgumentException("Event not found for ID: " + eventId));

        EventModality participationModality;

        if (event.getModality() == EventModality.HYBRID) {
          if (modality == null) {
            throw new IllegalArgumentException("Participation modality must be selected for hybrid events.");
          }

          participationModality = modality;
        } else {
          participationModality = event.getModality();
        }

        ParticipationService.INSTANCE.participate(
            personCpf,
            eventId,
            participationModality
        );

        writer.println("Person added to the event successfully!");
      } catch (Exception exception) {
        writer.println("Error adding person to event: " + exception.getMessage());
        if (AppConfig.DEBUG_MODE) {
          exception.printStackTrace(writer);
        }
      }
    }

    writer.println("\nPress Enter to return to the People Management menu...");
    provider.readLine("");
    ViewRepository.INSTANCE.getById(PeopleManagementView.NAME).ifPresent(Application::handleContextSwitch);
  }

}