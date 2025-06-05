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
public class Fair extends Event {

  @Override
  public List<FormField<?>> getSpecificFields() {
    return List.of();
  }

}
