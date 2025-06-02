package br.edu.ifba.entities.enums;

import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.Person;
import br.edu.ifba.entities.personas.External;
import br.edu.ifba.entities.personas.Student;
import br.edu.ifba.entities.personas.Teacher;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.stream.Stream;

@AllArgsConstructor
@Getter
public enum PersonType {
    EXTERNAL("External", External.class),
    STUDENT("Student", Student.class),
    TEACHER("Teacher", Teacher.class),;

    private final String label;
    private final Class<? extends Person> eventClass;

    @Override
    public String toString() {
        return label;
    }

    public static PersonType fromLabel(String label) {
        return Arrays.stream(PersonType.values())
                .filter(personType -> personType.getLabel().equalsIgnoreCase(label))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No PersonType found for label: " + label));
    }

    public static PersonType fromName(String name) {
        return Arrays.stream(PersonType.values())
                .filter(personType -> personType.name().equalsIgnoreCase(name))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No PersonType found for name: " + name));
    }

    public static Stream<PersonType> stream() {
        return Arrays.stream(PersonType.values());
    }
}
