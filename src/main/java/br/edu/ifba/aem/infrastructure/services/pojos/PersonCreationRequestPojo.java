package br.edu.ifba.aem.infrastructure.services.pojos;

import br.edu.ifba.aem.domain.enums.PersonType;
import java.time.LocalDate;

public record PersonCreationRequestPojo(String cpf, String name, LocalDate birthDate,
                                        PersonType type) {

}
