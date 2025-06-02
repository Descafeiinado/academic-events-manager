package br.edu.ifba.views.impl.forms;

import br.edu.ifba.applications.Application;
import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.enums.EventModality;
import br.edu.ifba.entities.enums.EventType;
import br.edu.ifba.entities.events.Course;
import br.edu.ifba.entities.events.Fair;
import br.edu.ifba.entities.events.Lecture;
import br.edu.ifba.entities.events.Workshop;
import br.edu.ifba.repositories.impl.EventRepository;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.form.FormField;
import br.edu.ifba.ui.pages.types.FormPage;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import br.edu.ifba.views.impl.MainView;

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

        fields.add(FormField.choice("modality", "Select event modality",
            EventModality.VIRTUAL, EventModality.class, EventModality::getLabel)
        );

        fields.add(FormField.integer("capacity", "Enter event capacity (overall)", 50));
        fields.add(FormField.freeText("description", "Enter event description (optional)", ""));
        fields.add(FormField.confirmation("confirmCreation", "Are you sure you want to create this event?", true));

        return fields;
    }

    @Override
    protected void onSubmit(InteractionProvider provider, Map<String, Object> results) {
        PrintWriter writer = provider.getWriter();
        writer.println("\n--- Form Submission Processing ---");

        if (results == null || results.isEmpty() || !Boolean.TRUE.equals(results.get("confirmCreation"))) {
            if (results != null && !results.isEmpty() && !Boolean.TRUE.equals(results.get("confirmCreation"))) {
                writer.println("Event creation cancelled by user.");
            } else {
                writer.println("Form was cancelled, an error occurred, or no data was entered.");
            }
        } else {
            EventType selectedEventType = (EventType) results.get("eventType");

            try {
                Event event = createEventInstance(selectedEventType, results);

                EventRepository.INSTANCE.save(event.getId(), event);
                EventRepository.INSTANCE.persist();

                writer.println(String.format("\nEvent #%d '%s' (%s) created successfully!", event.getId(), event.getTitle(), event.getType().getLabel()));
            } catch (Exception e) {
                Event.getSequentialIdProvider().rollback();

                writer.println("Error creating event instance: " + e.getMessage());

                e.printStackTrace(writer);
            }
        }

        writer.println("\nPress Enter to return to the main menu...");
        provider.readLine("");
        ViewRepository.INSTANCE.getById(MainView.NAME).ifPresent(Application::handleContextSwitch);
    }

    protected Event createEventInstance(EventType eventType, Map<String, Object> results) {
        try {
            if (eventType == null) {
                throw new IllegalArgumentException("Event type cannot be null");
            }

            return switch (eventType) {
                case COURSE -> Course.builder()
                        .title((String) results.get("eventName"))
                        .date((LocalDateTime) results.get("dateTime"))
                        .place((String) results.get("place"))
                        .type(eventType)
                        .modality((EventModality) results.get("modality"))
                        .capacity((Integer) results.get("capacity"))
                        .description((String) results.get("description"))
                        .duration((Integer) results.get("duration"))
                        .instructor((String) results.get("instructor"))
                        .build();
                case LECTURE -> Lecture.builder()
                        .title((String) results.get("eventName"))
                        .date((LocalDateTime) results.get("dateTime"))
                        .place((String) results.get("place"))
                        .type(eventType)
                        .modality((EventModality) results.get("modality"))
                        .capacity((Integer) results.get("capacity"))
                        .description((String) results.get("description"))
                        .speaker((String) results.get("speaker"))
                        .topic((String) results.get("topic"))
                        .build();
                case WORKSHOP -> Workshop.builder()
                        .title((String) results.get("eventName"))
                        .date((LocalDateTime) results.get("dateTime"))
                        .place((String) results.get("place"))
                        .type(eventType)
                        .modality((EventModality) results.get("modality"))
                        .capacity((Integer) results.get("capacity"))
                        .description((String) results.get("description"))
                        .materialsProvided((Boolean) results.get("materialsProvided"))
                        .numberOfSessions((Integer) results.get("numberOfSessions"))
                        .build();
                case FAIR -> Fair.builder()
                        .title((String) results.get("eventName"))
                        .date((LocalDateTime) results.get("dateTime"))
                        .place((String) results.get("place"))
                        .type(eventType)
                        .modality((EventModality) results.get("modality"))
                        .capacity((Integer) results.get("capacity"))
                        .description((String) results.get("description"))
                        .build();
            };

        } catch (Exception e) {
            throw new RuntimeException("Failed to create event instance for type: " + eventType, e);
        }
    }

    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new EventCreationFormView());
    }
}