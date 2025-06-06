package br.edu.ifba.aem.domain.exceptions;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;

public class AlreadyParticipatingException extends DomainException {

  public AlreadyParticipatingException(Person person, Event event, String modality) {
    super("Person with CPF " + person.getCpf() + " is already participating in the '" + modality
        + "' modality of the event " + event.getTitle() + " (ID: " + event.getId() + ").");
  }

}
