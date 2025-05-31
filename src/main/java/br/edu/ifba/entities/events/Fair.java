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
public class Fair extends Event {

    @Override
    public List<FormField<?>> getSpecificFields() {
        return List.of();
    }

}
