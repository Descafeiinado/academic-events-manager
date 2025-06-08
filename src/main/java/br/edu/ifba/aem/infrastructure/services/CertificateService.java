package br.edu.ifba.aem.infrastructure.services;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.exceptions.NoCertifiableEventsFoundException;
import br.edu.ifba.aem.domain.exceptions.PersonNotFoundException;
import br.edu.ifba.aem.domain.utils.Pair;
import br.edu.ifba.aem.domain.utils.cuid.CUID;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import br.edu.ifba.aem.infrastructure.services.pojos.FindCertifiableEventsPojo;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public class CertificateService {

  public static final String CERTIFICATE_BODY_FORMAT =
      "==================== CERTIFICATE ====================%n%n%s%n%nIdentifier: %s%n%n=====================================================";

  public static final CertificateService INSTANCE = new CertificateService();

  private final EventRepository eventRepository = EventRepository.INSTANCE;
  private final PersonRepository personRepository = PersonRepository.INSTANCE;

  public FindCertifiableEventsPojo findCertifiableEvents(String personCpf) {
    Person person = personRepository
        .getById(personCpf)
        .orElseThrow(() -> new PersonNotFoundException(personCpf));

    Predicate<Event> isCertifiable = event -> {
      boolean isParticipant = event.getInPersonParticipants().contains(personCpf) ||
          event.getVirtualParticipants().contains(personCpf);
      boolean isCompleted = event.getDate().isBefore(LocalDateTime.now());

      return isParticipant && isCompleted;
    };

    List<Event> certifiableEvents = person.getEventsParticipated().stream()
        .map(eventRepository::getById)
        .flatMap(Optional::stream)
        .filter(isCertifiable)
        .sorted(Comparator.comparing(Event::getDate).reversed())
        .toList();

    if (certifiableEvents.isEmpty()) {
      throw new NoCertifiableEventsFoundException(person);
    }

    return new FindCertifiableEventsPojo(
        person,
        certifiableEvents
    );
  }

  public Pair<String, String> generateCertificate(Person person, Event event) {
    CUID certificateId = CUID.randomCUID2(18);

    String template = event.getCertificateTemplate(person);
    String certificateContent = String.format(
        CERTIFICATE_BODY_FORMAT,
        template,
        certificateId
    );

    return Pair.of(
        certificateId.toString(),
        certificateContent
    );
  }

}
