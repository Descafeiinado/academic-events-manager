package br.edu.ifba.aem.domain.exceptions;

public class PersonNotFoundException extends DomainException {

  public PersonNotFoundException(String cpf) {
    super("Could not find person with CPF: " + cpf);
  }
}
