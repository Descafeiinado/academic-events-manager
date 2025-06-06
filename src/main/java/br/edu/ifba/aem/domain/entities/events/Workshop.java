package br.edu.ifba.aem.domain.entities.events;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.ui.components.FormField;
import java.util.List;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@SuperBuilder
public class Workshop extends Event {

  private boolean materialsProvided;
  private long numberOfSessions;

  @Override
  public List<FormField<?>> getSpecificFields() {
    return List.of(
        FormField.confirmation("materialsProvided", "Materials gonna be Provided", false),
        FormField.number("numberOfSessions", "Number of Sessions", 1)
    );
  }

}
