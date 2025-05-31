package br.edu.ifba.entities.events;

import br.edu.ifba.entities.Event;
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
public class Workshop extends Event {

    private boolean materialsProvided;
    private int numberOfSessions;

    @Override
    public List<FormField<?>> getSpecificFields() {
        return List.of(
            FormField.confirmation("materialsProvided", "Materials gonna be Provided", false),
            FormField.integer("numberOfSessions", "Number of Sessions", 1)
        );
    }

}
