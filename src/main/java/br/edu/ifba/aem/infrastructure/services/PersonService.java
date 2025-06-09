package br.edu.ifba.aem.infrastructure.services;

import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.entities.personas.External;
import br.edu.ifba.aem.domain.entities.personas.Student;
import br.edu.ifba.aem.domain.entities.personas.Teacher;
import br.edu.ifba.aem.domain.enums.PersonType;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import br.edu.ifba.aem.infrastructure.services.pojos.PersonCreationRequestPojo;

public class PersonService {

  public static final PersonService INSTANCE = new PersonService();

  private final PersonRepository personRepository = PersonRepository.INSTANCE;

  private Person createPersonInstance(PersonCreationRequestPojo request) {
    PersonType personType = request.type();

    try {
      if (personType == null) {
        throw new IllegalArgumentException("Person type cannot be null");
      }

      return switch (personType) {
        case EXTERNAL -> External.builder().cpf(request.cpf()).name(request.name())
            .birthDate(request.birthDate()).type(personType).build();
        case STUDENT ->
            Student.builder().cpf(request.cpf()).name(request.name()).birthDate(request.birthDate())
                .type(personType).build();
        case TEACHER ->
            Teacher.builder().cpf(request.cpf()).name(request.name()).birthDate(request.birthDate())
                .type(personType).build();
      };
    } catch (Exception exception) {
      throw new RuntimeException("Failed to create person instance for type: " + personType,
          exception);
    }
  }

  private void validatePersonCreationRequest(PersonCreationRequestPojo request) {
    if (request == null) {
      throw new IllegalArgumentException("Person creation request cannot be null");
    }

    if (request.cpf() == null || request.cpf().isBlank()) {
      throw new IllegalArgumentException("CPF cannot be null or blank");
    }

    if (request.name() == null || request.name().isBlank()) {
      throw new IllegalArgumentException("Name cannot be null or blank");
    }

    if (request.birthDate() == null) {
      throw new IllegalArgumentException("Birth date cannot be null");
    }

    if (request.type() == null) {
      throw new IllegalArgumentException("Person type cannot be null");
    }
  }

  public Person createPerson(PersonCreationRequestPojo request) {
    validatePersonCreationRequest(request);

    personRepository.getById(request.cpf()).ifPresent(existingPerson -> {
      throw new IllegalArgumentException("Person with CPF " + request.cpf() + " already exists.");
    });

    Person toCreate = createPersonInstance(request);

    try {
      personRepository.save(toCreate.getCpf(), toCreate);
    } catch (Exception e) {
      throw new RuntimeException("Error persisting person: " + e.getMessage(), e);
    }

    return toCreate;
  }
}
