package br.edu.ifba.aem.domain.entities.events;

import br.edu.ifba.aem.domain.entities.Event;
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

  private String speaker;
  private String topic;

  @Override
  public List<FormField<?>> getSpecificFields() {
    return List.of(
        PersonField.of("speaker", "Speaker's CPF"),
        FormField.text("topic", "Lecture Topic", null)
    );
  }

}
