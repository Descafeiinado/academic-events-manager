package br.edu.ifba.aem.domain.entities.events;

import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.utils.Pair;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.components.PersonField;
import java.util.List;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@SuperBuilder
public class Lecture extends Event {

  private static final PersonRepository personRepository = PersonRepository.INSTANCE;

  private String speaker;
  private String topic;

  public Person getSpeakerPerson() {
    if (speaker == null || speaker.isBlank()) {
      return null;
    }

    return personRepository.getById(speaker).orElse(null);
  }

  @Override
  public List<FormField<?>> getSpecificFields() {
    return List.of(PersonField.of("speaker", "Speaker's CPF"),
        FormField.text("topic", "Lecture Topic", null));
  }

  @Override
  public List<Pair<String, String>> getDescriptiveFields() {
    return List.of(Pair.of("Speaker", getSpeakerPerson() != null ? getSpeakerPerson().getName()
            : "Unknown Speaker with CPF " + GlobalScope.CPF_REDACTOR.apply(getSpeaker())),
        Pair.of("Topic", topic != null ? topic : "Not specified"));
  }

  @Override
  public String getCertificateTemplate(Person person) {
    Person speakerPerson = getSpeakerPerson();

    return String.format(
        "Certificate of Attendance\n\nThis certifies that %s attended the lecture \"%s\" (#%d) presented by %s on the topic \"%s\", held on %s at %s.",
        person.getName(), getTitle(), getId(), speakerPerson != null ? speakerPerson.getName()
            : "Unknown Instructor with CPF " + GlobalScope.CPF_REDACTOR.apply(getSpeaker()),
        getTopic(), getDate().toLocalDate(), getPlace());
  }

}
