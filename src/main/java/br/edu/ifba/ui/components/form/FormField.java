package br.edu.ifba.ui.components.form;

import lombok.Data;

import java.util.function.Function;
import java.util.function.Predicate;

@Data
public class FormField<T> {
    private final String name;
    private final String message;
    private final T defaultValue;
    private final Function<String, T> parser;
    private final Predicate<String> validator;
}
