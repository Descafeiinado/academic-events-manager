package br.edu.ifba.aem.infrastructure.services;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.events.Course;
import br.edu.ifba.aem.domain.entities.events.Lecture;
import br.edu.ifba.aem.domain.entities.personas.External;
import br.edu.ifba.aem.domain.entities.personas.Teacher;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;

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
      External speaker = (External) personRepository
          .getById(lecture.getSpeaker())
          .orElseThrow(() -> new IllegalArgumentException(
              "Speaker with CPF " + lecture.getSpeaker() + " not found"));

      speaker.getLecturesPresented().add(lecture.getId());

      personRepository.save(speaker.getCpf(), speaker);
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

}
