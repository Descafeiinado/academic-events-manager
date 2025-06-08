package br.edu.ifba.aem.domain.exceptions;

import br.edu.ifba.aem.domain.entities.Event;

public class EventFullException extends DomainException {

  public EventFullException(Event event, String modality) {
    super("Event " + event.getTitle() + " (ID: " + event.getId()
        + ") is already have it's maximum capacity reached for the '" + modality
        + "' modality.");
  }

}
