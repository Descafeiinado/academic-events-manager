package br.edu.ifba.aem.infrastructure.services.pojos;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import java.util.List;

public record FindCertifiableEventsPojo(Person person, List<Event> events) {

}
