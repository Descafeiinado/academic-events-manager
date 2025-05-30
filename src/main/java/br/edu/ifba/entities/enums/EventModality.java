package br.edu.ifba.entities.enums;

import java.util.Arrays;
import java.util.stream.Stream;

public enum EventModality {
    IN_PERSON("In-person"),
    VIRTUAL("Virtual"),
    HYBRID("Hybrid");

    private final String label;

    EventModality(String label) {
        this.label = label;
    }

    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {
        return label;
    }

    public static Stream<EventModality> stream() {
        return Arrays.stream(EventModality.values());
    }
}
