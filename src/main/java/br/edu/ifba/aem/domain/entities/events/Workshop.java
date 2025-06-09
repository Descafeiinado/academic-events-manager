package br.edu.ifba.aem.domain.entities.events;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.utils.Pair;
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
        FormField.number("numberOfSessions", "Number of Sessions", 1));
  }

  @Override
  public List<Pair<String, String>> getDescriptiveFields() {
    return List.of(Pair.of("Materials Provided", materialsProvided ? "Yes" : "No"),
        Pair.of("Number of Sessions", String.valueOf(numberOfSessions)));
  }

  @Override
  public String getCertificateTemplate(Person person) {
    return String.format(
        "Certificate of Participation\n\nThis certifies that %s has participated in the workshop \"%s\" (#%d) with %d session(s)%s, held on %s at %s.",
        person.getName(), getTitle(), getId(), getNumberOfSessions(),
        isMaterialsProvided() ? " (materials were provided)" : "", getDate().toLocalDate(),
        getPlace());
  }

}
