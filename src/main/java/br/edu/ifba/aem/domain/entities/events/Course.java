package br.edu.ifba.aem.domain.entities.events;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.enums.PersonType;
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
public class Course extends Event {

  private String instructor;
  private int duration; // Duration in hours

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

}
