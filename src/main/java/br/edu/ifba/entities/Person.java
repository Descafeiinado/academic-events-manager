package br.edu.ifba.entities;

import br.edu.ifba.entities.enums.PersonType;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@SuperBuilder
public abstract class Person {

    private String cpf;
    private String name;
    private LocalDate birthDate;

    private PersonType type;

    private final List<Long> eventsParticipated = new ArrayList<>();

}
