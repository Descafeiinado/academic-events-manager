package br.edu.ifba.aem.domain.entities;

import br.edu.ifba.aem.domain.enums.PersonType;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@NoArgsConstructor
@SuperBuilder
public abstract class Person {

  private final List<Long> eventsParticipated = new ArrayList<>();
  private String cpf;
  private String name;
  private LocalDate birthDate;
  private PersonType type;

}
