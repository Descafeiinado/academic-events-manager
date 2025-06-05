package br.edu.ifba.aem.infrastructure.service;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.domain.exceptions.EventNotFoundException;
import br.edu.ifba.aem.domain.exceptions.PersonNotFoundException;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;

public class ParticipationService {

  public static final ParticipationService INSTANCE = new ParticipationService();

  private final EventRepository eventRepository = EventRepository.INSTANCE;
  private final PersonRepository personRepository = PersonRepository.INSTANCE;

  public void participate(String personCpf, Long eventId, EventModality participationModality) {
    // Fetch the Person object or throw PersonNotFoundException if not found.
    // The initial .exists(personCpf) check is consolidated here.
    Person person = personRepository
        .getById(personCpf) // Assuming this method exists and returns Optional<Person>
        .orElseThrow(() -> new PersonNotFoundException(personCpf));

    // Fetch the Event object or throw EventNotFoundException if not found.
    // The initial .exists(eventId) check is consolidated here.
    Event event = eventRepository
        .getById(eventId) // Assuming this method exists and returns Optional<Event>
        .orElseThrow(() -> new EventNotFoundException(eventId));

    if (participationModality == null) {
//      throw new InvalidEventModalityException(
//          "Event modality for event ID " + eventId + " is not set."
//      );
      throw new IllegalArgumentException(
          "Event modality for event ID " + eventId + " is not set."
      );
    }

    // Logic for adding participant based on event modality
    switch (participationModality) {
      case IN_PERSON:
        if (event.getInPersonParticipants().contains(personCpf)) {
//          throw new AlreadyParticipatingException(
//              personCpf,
//              eventId,
//              "in-person"
//          );
          throw new IllegalArgumentException(
              "Person with CPF " + personCpf + " is already participating in the in-person modality event with ID " + eventId
          );
        }
        // Check capacity only if it's set (not null)
        if (
            event.getCapacity() != null &&
                event.getInPersonParticipants().size() >= event.getCapacity()
        ) {
//          throw new EventFullException(
//              eventId,
//              "In-person capacity reached."
//          );
          throw new IllegalArgumentException(
              "In-person capacity for event ID " + eventId + " has been reached."
          );
        }
        event.getInPersonParticipants().add(personCpf);
        break;
      case VIRTUAL:
        if (event.getVirtualParticipants().contains(personCpf)) {
//          throw new AlreadyParticipatingException(
//              personCpf,
//              eventId,
//              "virtual"
//          );
          throw new IllegalArgumentException(
              "Person with CPF " + personCpf + " is already participating in the virtual modality event with ID " + eventId
          );
        }
        event.getVirtualParticipants().add(personCpf);
        break;
      default:
        // This case handles any unexpected or unsupported modalities.
//        throw new InvalidEventModalityException(
//            "Unsupported event modality: " +
//                modality +
//                " for event ID " +
//                eventId
//        );
        throw new IllegalArgumentException(
            "Unsupported event modality: " + participationModality + " for event ID " + eventId
        );
    }

    // Add the event to the person's list of participated events, if not already there.
    // This ensures idempotency at the person's event list level.
    if (!person.getEventsParticipated().contains(eventId)) {
      person.getEventsParticipated().add(eventId);
    }

    // Assuming that changes to the `event` and `person` objects are automatically
    // persisted by the repositories if they manage in-memory collections or have
    // appropriate persistence mechanisms. If explicit save/update calls are needed,
    // they would go here, e.g.:
     eventRepository.save(event.getId(), event);
     personRepository.save(person.getCpf(), person);
  }
}
