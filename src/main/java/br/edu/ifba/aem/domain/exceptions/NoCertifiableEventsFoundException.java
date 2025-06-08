package br.edu.ifba.aem.domain.exceptions;

import br.edu.ifba.aem.domain.entities.Person;

public class NoCertifiableEventsFoundException extends DomainException {

  public NoCertifiableEventsFoundException(Person person) {
    super("Person with CPF " + person.getCpf() + " has no certifiable events.");
  }

}
