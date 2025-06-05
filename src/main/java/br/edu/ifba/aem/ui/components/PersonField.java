package br.edu.ifba.aem.ui.components;

import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import java.util.Collections;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import lombok.Getter;

@Getter
public class PersonField extends FormField<String> {

  private final Function<String, Optional<Person>> displayPersonRetriever;

  public PersonField(String name, String label, Function<Person, Boolean> personBusinessValidator) {
    super(name, label, null, createPersonParser(personBusinessValidator),
        createPersonValidator(personBusinessValidator), FieldType.PERSON, Collections.emptyMap(),
        Collections.emptyMap());
    this.displayPersonRetriever = createInternalDisplayPersonRetriever();
  }

  private static Function<String, String> createPersonParser(
      Function<Person, Boolean> personBusinessValidator) {
    return cpfInput -> {
      if (cpfInput == null || cpfInput.isBlank()) {
        return null;
      }

      String cleanedCpf = FormField.DEFAULT_CPF_PARSER.apply(cpfInput);

      PersonRepository personRepository = PersonRepository.INSTANCE;
      Optional<Person> personOptional = personRepository.getById(cleanedCpf);

      if (personOptional.isEmpty()) {
        throw new IllegalArgumentException("Person not found for CPF: " + cleanedCpf);
      }

      Person person = personOptional.get();

      if (personBusinessValidator != null) {
        try {
          if (!personBusinessValidator.apply(person)) {
            throw new IllegalArgumentException(
                "Person failed business validation: " + person.getName());
          }
        } catch (IllegalArgumentException exception) {
          throw exception;
        } catch (Exception exception) {
          throw new IllegalArgumentException(
              "Error during person business validation: " + exception.getMessage(), exception);
        }
      }
      return cleanedCpf;
    };
  }

  private static Predicate<String> createPersonValidator(
      Function<Person, Boolean> personBusinessValidator) {
    return cpfInput -> {
      if (cpfInput == null || cpfInput.isBlank()) {
        return false;
      }

      if (!FormField.DEFAULT_CPF_VALIDATOR.test(cpfInput)) {
        return false;
      }

      String cleanedCpf;

      try {
        cleanedCpf = FormField.DEFAULT_CPF_PARSER.apply(cpfInput);
      } catch (IllegalArgumentException exception) {
        return false;
      }

      PersonRepository personRepository = PersonRepository.INSTANCE;
      Optional<Person> personOptional = personRepository.getById(cleanedCpf);

      if (personOptional.isEmpty()) {
        return false;
      }

      if (personBusinessValidator != null) {
        try {
          return personBusinessValidator.apply(personOptional.get());
        } catch (Exception exception) {
          return false;
        }
      }

      return true;
    };
  }

  private static Function<String, Optional<Person>> createInternalDisplayPersonRetriever() {
    return rawCpfInput -> {
      if (rawCpfInput == null || rawCpfInput.isBlank()) {
        return Optional.empty();
      }

      String cleanedCpfForLookup;

      try {
        cleanedCpfForLookup = FormField.DEFAULT_CPF_PARSER.apply(rawCpfInput);
      } catch (IllegalArgumentException exception) {
        return Optional.empty();
      }

      PersonRepository personRepository = PersonRepository.INSTANCE;

      return personRepository.getById(cleanedCpfForLookup);
    };
  }

  public static PersonField of(String name, String label) {
    return new PersonField(name, label, null);
  }

  public static PersonField of(String name, String label,
      Function<Person, Boolean> personBusinessValidator) {
    return new PersonField(name, label, personBusinessValidator);
  }

  public Function<String, Optional<Person>> getDisplayPersonRetrieverFunction() {
    return this.displayPersonRetriever;
  }
}
