package br.edu.ifba.entities.enums;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.Arrays;
import java.util.stream.Stream;

@AllArgsConstructor
@Getter
public enum EventModality {
    IN_PERSON("In-person"),
    VIRTUAL("Virtual"),
    HYBRID("Hybrid");

    private final String label;

    @Override
    public String toString() {
        return label;
    }

    public static Stream<EventModality> stream() {
        return Arrays.stream(EventModality.values());
    }
}
