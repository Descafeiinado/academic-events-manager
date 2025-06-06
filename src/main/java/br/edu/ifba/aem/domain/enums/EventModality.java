package br.edu.ifba.aem.domain.enums;

import br.edu.ifba.aem.ui.components.FormField;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum EventModality {
  IN_PERSON("In-person"),
  VIRTUAL("Virtual"),
  HYBRID("Hybrid");

  private final String label;

  public List<FormField<?>> getSpecificFieldsFromType() {

    if (this == HYBRID) {
      return List.of(
          FormField.number("inPersonCapacity", "Enter event In-person Capacity", 25),
          FormField.number("virtualCapacity", "Enter event Virtual Capacity", 25)
      );
    }

    return List.of(
        FormField.number("capacity", "Enter event capacity (overall)", 50)
    );
  }

  @Override
  public String toString() {
    return label;
  }

}
