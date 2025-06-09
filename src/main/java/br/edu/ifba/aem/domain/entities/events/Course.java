package br.edu.ifba.aem.domain.entities.events;

import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.entities.interfaces.ParticipationRestrictive;
import br.edu.ifba.aem.domain.entities.personas.Teacher;
import br.edu.ifba.aem.domain.enums.PersonType;
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
public class Course extends Event implements ParticipationRestrictive {

  private static final PersonRepository personRepository = PersonRepository.INSTANCE;

  private String instructor;
  private long duration; // Duration in hours

  public Teacher getInstructorPerson() {
    if (instructor == null || instructor.isBlank()) {
      return null;
    }

    return personRepository.getById(instructor)
        .filter(person -> person.getType() == PersonType.TEACHER)
        .map(Teacher.class::cast)
        .orElse(null);
  }

  @Override
  public List<FormField<?>> getSpecificFields() {
    return List.of(
        PersonField.of("instructor", "Instructor's CPF", person -> {
          if (person.getType() != PersonType.TEACHER) {
            throw new IllegalArgumentException("The instructor must be a teacher.");
          }

          return true;
        }),
        FormField.number("duration", "Duration in hours", 4)
    );
  }

  @Override
  public String getCertificateTemplate(Person person) {
    Teacher instructorPerson = getInstructorPerson();

    return String.format(
        "Certificate of Participation\n\nThis certifies that %s has successfully participated in the course \"%s\" (#%d) instructed by %s, with a total duration of %d hours, held on %s at %s.",
        person.getName(),
        getTitle(),
        getId(),
        instructorPerson != null ? instructorPerson.getName()
            : "Unknown Instructor with CPF " + GlobalScope.CPF_REDACTOR.apply(getInstructor()),
        getDuration(),
        getDate().toLocalDate(),
        getPlace()
    );
  }

  @Override
  public List<Pair<String, String>> getDescriptiveFields() {
    return List.of(
        Pair.of("Instructor", getInstructorPerson() != null ? getInstructorPerson().getName()
            : "Unknown Instructor with CPF " + GlobalScope.CPF_REDACTOR.apply(getInstructor())),
        Pair.of("Duration", String.format("%d hours", getDuration()))
    );
  }

  @Override
  public void checkPersonCapability(Person person) {
    if (person.getType() != PersonType.STUDENT) {
      throw new IllegalArgumentException("Only students can participate in courses.");
    }
  }

}
