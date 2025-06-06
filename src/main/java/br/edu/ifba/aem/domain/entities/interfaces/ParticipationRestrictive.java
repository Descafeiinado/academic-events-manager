package br.edu.ifba.aem.domain.entities.interfaces;

import br.edu.ifba.aem.domain.entities.Person;

public interface ParticipationRestrictive {

  void checkPersonCapability(Person person);
}
