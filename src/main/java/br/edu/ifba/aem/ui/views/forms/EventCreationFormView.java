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
import br.edu.ifba.aem.domain.models.EventCapacity;
import br.edu.ifba.aem.infrastructure.services.EventService;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.views.EventManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.PrintWriter;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EventCreationFormView extends FormPage implements View {

  public static final String NAME = "NEW_EVENT_FORM_VIEW";

  public EventCreationFormView() {
    super("Create New Event");
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new EventCreationFormView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(FormField.text("eventName", "Enter event name", "My Awesome Event"));
    fields.add(FormField.dateTime("dateTime", "Enter event date and time",
        LocalDateTime.now().plusDays(7).withHour(10).withMinute(0)
    ));
    fields.add(FormField.text("place", "Enter event place", "Online Platform"));

    // --- Define conditional fields ---
    Map<String, List<FormField<?>>> eventTypeConditionalChildren = new HashMap<>();

    for (EventType eventType : EventType.values()) {
      eventTypeConditionalChildren.put(eventType.name(), eventType.getSpecificFieldsFromType());
    }

    fields.add(FormField.choice("eventType", "Select event type",
        EventType.LECTURE, EventType.class, EventType::getLabel,
        eventTypeConditionalChildren)
    );

    Map<String, List<FormField<?>>> modalityConditionalChildren = new HashMap<>();

    for (EventModality modality : EventModality.values()) {
      modalityConditionalChildren.put(modality.name(), modality.getSpecificFieldsFromType());
    }

    fields.add(FormField.choice("modality", "Select event modality",
        EventModality.VIRTUAL, EventModality.class, EventModality::getLabel,
        modalityConditionalChildren)
    );

    fields.add(FormField.freeText("description", "Enter event description (optional)", ""));
    fields.add(
        FormField.confirmation("confirmCreation", "Are you sure you want to create this event?",
            true));

    return fields;
  }

  @Override
  public void onInterruptScreen(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\n--- Event Creation Interrupted ---");
    writer.println("Event creation form was interrupted or cancelled.");
    writer.println("Returning to the event management menu...");

    ViewRepository.INSTANCE.getById(EventManagementView.NAME)
        .ifPresent(Application::handleContextSwitch);
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
        writer.println("Event creation cancelled by user.");
      }

      promptReturnToLatestMenu(provider);
      return;
    }

    EventType selectedEventType = (EventType) results.get("eventType");

    try {
      Event event = EventService.INSTANCE.createEvent(
          createEventObject(selectedEventType, results));

      writer.println(String.format("\nEvent #%d '%s' (%s) created successfully!", event.getId(),
          event.getTitle(), event.getType().getLabel()));
    } catch (Exception exception) {
      writer.println("Error creating event instance: " + exception.getMessage());

      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace(writer);
      }
    }

    promptReturnToLatestMenu(provider);
  }

  protected Event createEventObject(EventType eventType, Map<String, Object> results) {
    try {
      if (eventType == null) {
        throw new IllegalArgumentException("Event type cannot be null");
      }

      EventModality modality = (EventModality) results.get("modality");
      EventCapacity.EventCapacityBuilder capacityBuilder = EventCapacity.builder()
          .inPersonCapacity(0L)
          .virtualCapacity(0L);

      switch (modality) {
        case VIRTUAL -> capacityBuilder.virtualCapacity((Long) results.get("capacity"));
        case IN_PERSON -> capacityBuilder.inPersonCapacity((Long) results.get("capacity"));
        case HYBRID -> capacityBuilder
            .inPersonCapacity((Long) results.get("inPersonCapacity"))
            .virtualCapacity((Long) results.get("virtualCapacity"));
      }

      return switch (eventType) {
        case COURSE -> Course.builder()
            .title((String) results.get("eventName"))
            .date((LocalDateTime) results.get("dateTime"))
            .place((String) results.get("place"))
            .type(eventType)
            .modality(modality)
            .capacity(capacityBuilder.build())
            .description((String) results.get("description"))
            .duration((Long) results.get("duration"))
            .instructor((String) results.get("instructor"))
            .build();
        case LECTURE -> Lecture.builder()
            .title((String) results.get("eventName"))
            .date((LocalDateTime) results.get("dateTime"))
            .place((String) results.get("place"))
            .type(eventType)
            .modality(modality)
            .capacity(capacityBuilder.build())
            .description((String) results.get("description"))
            .speaker((String) results.get("speaker"))
            .topic((String) results.get("topic"))
            .build();
        case WORKSHOP -> Workshop.builder()
            .title((String) results.get("eventName"))
            .date((LocalDateTime) results.get("dateTime"))
            .place((String) results.get("place"))
            .type(eventType)
            .modality(modality)
            .capacity(capacityBuilder.build())
            .description((String) results.get("description"))
            .materialsProvided((Boolean) results.get("materialsProvided"))
            .numberOfSessions((Long) results.get("numberOfSessions"))
            .build();
        case FAIR -> Fair.builder()
            .title((String) results.get("eventName"))
            .date((LocalDateTime) results.get("dateTime"))
            .place((String) results.get("place"))
            .type(eventType)
            .modality(modality)
            .capacity(capacityBuilder.build())
            .description((String) results.get("description"))
            .build();
      };

    } catch (Exception exception) {
      throw new RuntimeException("Failed to create event instance for type: " + eventType,
          exception);
    }
  }

  private void promptReturnToLatestMenu(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\nPress Enter to return to the Event Management menu...");
    provider.readLine("");

    ViewRepository.INSTANCE.getById(EventManagementView.NAME)
        .ifPresent(Application::handleContextSwitch);
  }

}