package br.edu.ifba.aem.infrastructure.services;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.entities.events.Course;
import br.edu.ifba.aem.domain.entities.events.Lecture;
import br.edu.ifba.aem.domain.entities.personas.External;
import br.edu.ifba.aem.domain.entities.personas.Teacher;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.domain.exceptions.NoEventsFoundException;
import br.edu.ifba.aem.domain.models.DateRange;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import java.time.LocalDate;
import java.util.List;

public class EventService {

  public static final EventService INSTANCE = new EventService();

  private final EventRepository eventRepository = EventRepository.INSTANCE;
  private final PersonRepository personRepository = PersonRepository.INSTANCE;

  public Event createEvent(Event toCreate) {
    if (toCreate instanceof Course course) {
      Teacher teacher = (Teacher) personRepository
          .getById(course.getInstructor())
          .orElseThrow(() -> new IllegalArgumentException(
              "Teacher with CPF " + course.getInstructor() + " not found"));

      teacher.getCoursesTaught().add(course.getId());

      personRepository.save(teacher.getCpf(), teacher);
    }

    if (toCreate instanceof Lecture lecture) {
      Person speaker = personRepository
          .getById(lecture.getSpeaker())
          .orElseThrow(() -> new IllegalArgumentException(
              "Speaker with CPF " + lecture.getSpeaker() + " not found"));

      if (speaker instanceof External external) {
        external.getLecturesPresented().add(lecture.getId());

        personRepository.save(external.getCpf(), external);
      }
    }

    saveEvent(toCreate);

    return toCreate;
  }

  public void saveEvent(Event event) {
    try {
      eventRepository.save(event.getId(), event);
    } catch (Exception e) {
      Event.getSequentialIdProvider().rollback();

      throw new RuntimeException("Error creating event: " + e.getMessage(), e);
    }
  }

  public List<Event> findEventsByDate(LocalDate date) {
    List<Event> events = eventRepository.stream()
        .filter(event -> event.getDate().toLocalDate().equals(date))
        .toList();

    if (events.isEmpty()) {
      throw new NoEventsFoundException();
    }

    return events;
  }

  public List<Event> findEventsByDateRange(DateRange dateRange) {
    List<Event> events = eventRepository.stream()
        .filter(event -> dateRange.contains(event.getDate().toLocalDate()))
        .toList();

    if (events.isEmpty()) {
      throw new NoEventsFoundException();
    }

    return events;
  }

  public List<Event> findEventsByType(EventType eventType) {
    List<Event> events = eventRepository.stream()
        .filter(event -> event.getType() == eventType)
        .toList();

    if (events.isEmpty()) {
      throw new NoEventsFoundException();
    }

    return events;
  }

  public List<Event> findAllEvents() {
    List<Event> events = eventRepository.stream().toList();

    if (events.isEmpty()) {
      throw new NoEventsFoundException();
    }

    return events;
  }

}
