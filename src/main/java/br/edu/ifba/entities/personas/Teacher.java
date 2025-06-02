package br.edu.ifba.entities.personas;

import br.edu.ifba.entities.Person;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@SuperBuilder
public class Teacher extends Person {

    private List<Long> coursesTaught = List.of();

}
