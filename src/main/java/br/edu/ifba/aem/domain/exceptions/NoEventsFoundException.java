package br.edu.ifba.aem.domain.exceptions;

public class NoEventsFoundException extends DomainException {

  public NoEventsFoundException() {
    super("No events found matching the specified criteria.");
  }

}
