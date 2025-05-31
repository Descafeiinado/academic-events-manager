package br.edu.ifba.entities.enums;

import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.events.Course;
import br.edu.ifba.entities.events.Fair;
import br.edu.ifba.entities.events.Lecture;
import br.edu.ifba.entities.events.Workshop;
import br.edu.ifba.ui.components.form.FormField;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

@Getter
@AllArgsConstructor
public enum EventType {

    COURSE("Course", Course.class), // Curso
    LECTURE("Lecture", Lecture.class), //Palestra
    WORKSHOP("Workshop", Workshop.class), // Oficina
    FAIR("Fair", Fair.class); // Feira

    private final String label;
    private final Class<? extends Event> eventClass;

    public List<FormField<?>> getSpecificFieldsFromType() {
        try {
            Event eventInstance = eventClass.getDeclaredConstructor().newInstance();
            return eventInstance.getSpecificFields();
        } catch (InstantiationException | IllegalAccessException |
                 InvocationTargetException | NoSuchMethodException e) {
            e.printStackTrace();
            return Collections.emptyList();
        }
    }

    public static EventType fromLabel(String label) {
        return Arrays.stream(EventType.values())
                .filter(eventType -> eventType.getLabel().equalsIgnoreCase(label))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No EventType found for label: " + label));
    }

    public static EventType fromName(String name) {
        return Arrays.stream(EventType.values())
                .filter(eventType -> eventType.name().equalsIgnoreCase(name))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No EventType found for name: " + name));
    }

    public static Stream<EventType> stream() {
        return Arrays.stream(EventType.values());
    }
}
