package br.edu.ifba.aem.domain.entities.personas;

import br.edu.ifba.aem.domain.entities.Person;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@SuperBuilder
public class Student extends Person {

}
