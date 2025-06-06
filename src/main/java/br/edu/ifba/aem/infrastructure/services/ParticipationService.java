package br.edu.ifba.aem.infrastructure.services;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.entities.interfaces.ParticipationRestrictive;
import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.domain.exceptions.AlreadyParticipatingException;
import br.edu.ifba.aem.domain.exceptions.EventFullException;
import br.edu.ifba.aem.domain.exceptions.EventNotFoundException;
import br.edu.ifba.aem.domain.exceptions.InvalidEventModalityException;
import br.edu.ifba.aem.domain.exceptions.PersonNotFoundException;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;

public class ParticipationService {

  public static final ParticipationService INSTANCE = new ParticipationService();

  private final EventRepository eventRepository = EventRepository.INSTANCE;
  private final PersonRepository personRepository = PersonRepository.INSTANCE;

  public void participate(String personCpf, Long eventId, EventModality participationModality) {
    Person person = personRepository
        .getById(personCpf)
        .orElseThrow(() -> new PersonNotFoundException(personCpf));

    Event event = eventRepository
        .getById(eventId)
        .orElseThrow(() -> new EventNotFoundException(eventId));

    if (event instanceof ParticipationRestrictive participationRestrictive) {
      participationRestrictive.checkPersonCapability(person);
    }

    if (participationModality == null) {
      throw new InvalidEventModalityException(
          "Event modality for event ID " + eventId + " is not set."
      );
    }

    switch (participationModality) {
      case IN_PERSON:
        if (event.getInPersonParticipants().contains(personCpf)) {
          throw new AlreadyParticipatingException(
              person,
              event,
              "in-person"
          );
        }

        if (
            event.getCapacity() != null &&
                event.getInPersonParticipants().size() >= event.getCapacity().getInPersonCapacity()
        ) {
          throw new EventFullException(
              event,
              "In-person capacity reached."
          );
        }

        event.getInPersonParticipants().add(personCpf);
        break;
      case VIRTUAL:
        if (event.getVirtualParticipants().contains(personCpf)) {
          throw new AlreadyParticipatingException(
              person,
              event,
              "virtual"
          );
        }

        if (
            event.getCapacity() != null &&
                event.getVirtualParticipants().size() >= event.getCapacity().getVirtualCapacity()
        ) {
          throw new EventFullException(
              event,
              "Virtual capacity reached."
          );
        }

        event.getVirtualParticipants().add(personCpf);
        break;
      default:
        throw new IllegalArgumentException(
            "Unsupported event modality: " + participationModality + " for event ID " + eventId
        );
    }

    if (!person.getEventsParticipated().contains(eventId)) {
      person.getEventsParticipated().add(eventId);
    }

    eventRepository.save(event.getId(), event);
    personRepository.save(person.getCpf(), person);
  }
}
