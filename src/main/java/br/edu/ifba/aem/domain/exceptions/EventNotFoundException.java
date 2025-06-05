package br.edu.ifba.aem.domain.exceptions;

public class EventNotFoundException extends DomainException {

  public EventNotFoundException(Long id) {
    super("Event with ID " + id + " not found.");
  }
}
