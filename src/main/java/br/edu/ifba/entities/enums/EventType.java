package br.edu.ifba.entities.enums;

import java.util.Arrays;
import java.util.stream.Stream;

public enum EventType {

    COURSE("Course"), // Curso
    LECTURE("Lecture"), //Palestra
    WORKSHOP("Workshop"), // Oficina
    FAIR("Fair"); // Feira

    private final String label;

    EventType(String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    public static Stream<EventType> stream() {
        return Arrays.stream(EventType.values());
    }
}
