package br.edu.ifba.aem.domain.entities.personas;

import br.edu.ifba.aem.domain.entities.Person;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import java.util.List;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@SuperBuilder
public class External extends Person {

  private List<Long> lecturesPresented = List.of();

}
