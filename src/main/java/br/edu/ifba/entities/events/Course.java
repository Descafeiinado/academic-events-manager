package br.edu.ifba.entities.events;

import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.enums.PersonType;
import br.edu.ifba.ui.components.form.FormField;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

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
                FormField.person("instructor", "Instructor's CPF", person -> {
                    if (person.getType() != PersonType.TEACHER) {
                        throw new IllegalArgumentException("The instructor must be a teacher.");
                    }

                    return true;
                }),
                FormField.integer("duration", "Duration in hours", 4)
        );
    }

}
