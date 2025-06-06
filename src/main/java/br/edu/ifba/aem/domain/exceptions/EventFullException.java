package br.edu.ifba.aem.domain.exceptions;

import br.edu.ifba.aem.domain.entities.Event;

public class EventFullException extends DomainException {

  public EventFullException(Event event, String modality) {
    super("Event " + event.getTitle() + " (ID: " + event.getId() + ") is full for the '" + modality
        + "' modality.");
  }

}
