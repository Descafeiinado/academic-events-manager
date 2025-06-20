package br.edu.ifba.aem.domain.entities;

import br.edu.ifba.aem.domain.enums.PersonType;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@NoArgsConstructor
@SuperBuilder
public abstract class Person {

  private String cpf;
  private String name;
  private LocalDate birthDate;
  private PersonType type;

  @Builder.Default
  private final List<Long> eventsParticipated = new ArrayList<>();

  @Builder.Default
  private LocalDateTime createdAt = LocalDateTime.now();

}
