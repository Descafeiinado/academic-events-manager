package br.edu.ifba.aem.domain.enums;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.events.Course;
import br.edu.ifba.aem.domain.entities.events.Fair;
import br.edu.ifba.aem.domain.entities.events.Lecture;
import br.edu.ifba.aem.domain.entities.events.Workshop;
import br.edu.ifba.aem.ui.components.FormField;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum EventType {

  COURSE("Course", Course.class), // Curso
  LECTURE("Lecture", Lecture.class), //Palestra
  WORKSHOP("Workshop", Workshop.class), // Oficina
  FAIR("Fair", Fair.class); // Feira

  private final String label;
  private final Class<? extends Event> eventClass;

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

  public List<FormField<?>> getSpecificFieldsFromType() {
    try {
      Event eventInstance = eventClass.getDeclaredConstructor().newInstance();
      return eventInstance.getSpecificFields();
    } catch (InstantiationException | IllegalAccessException |
             InvocationTargetException | NoSuchMethodException exception) {
      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace();
      }
      return Collections.emptyList();
    }
  }

  @Override
  public String toString() {
    return label;
  }
}
