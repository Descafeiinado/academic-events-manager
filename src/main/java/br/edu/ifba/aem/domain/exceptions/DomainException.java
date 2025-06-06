package br.edu.ifba.aem.domain.exceptions;

import br.edu.ifba.aem.application.AppConfig;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class DomainException extends RuntimeException {

  private boolean stackTraceable = AppConfig.DEBUG_MODE;

  public DomainException(String message) {
    super(message);
  }

  @Override
  public synchronized Throwable fillInStackTrace() {
    if (stackTraceable) {
      return super.fillInStackTrace();
    }

    return this;
  }

}
