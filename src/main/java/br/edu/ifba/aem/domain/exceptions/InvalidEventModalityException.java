package br.edu.ifba.aem.domain.exceptions;

public class InvalidEventModalityException extends DomainException {

  public InvalidEventModalityException(String message) {
    this(message, false);
  }

  public InvalidEventModalityException(String message, boolean stackTraceable) {
    super(message);

    setStackTraceable(stackTraceable);
  }

}
