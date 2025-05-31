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
public class Lecture extends Event {

    private String speaker;
    private String topic;

    @Override
    public List<FormField<?>> getSpecificFields() {
        return List.of(
            FormField.cpf("speaker", "Speaker's Name", null),
            FormField.text("topic", "Lecture Topic", null)
        );
    }

}
