package br.edu.ifba.entities.enums;

import java.util.List;
import java.util.stream.Stream;

public enum ConfirmationValue {
    YES(List.of("Y, YES")),
    NO(List.of("N, NO"));

    private final List<String> possibleValues;

    ConfirmationValue(List<String> possibleValues) {
        this.possibleValues = possibleValues;
    }

    public List<String> getPossibleValues() {
        return possibleValues;
    }

    public static ConfirmationValue fromString(String value) {
        return Stream.of(ConfirmationValue.values())
                .filter(v -> v.getPossibleValues().contains(value.toUpperCase()))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Invalid confirmation value: " + value));
    }
}
