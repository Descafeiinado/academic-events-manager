package br.edu.ifba.ui.components.form;

import br.edu.ifba.GlobalScope;
import lombok.Getter;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

@Getter
public class FormField<T> {
    private final String name;
    private final String label;
    private final T defaultValue;
    private final Function<String, T> parser;
    private final Predicate<String> validator;
    private final FieldType fieldType;
    private final Map<String, String> options;
    private final Map<String, List<FormField<?>>> conditionalChildren;

    public FormField(String name, String label, T defaultValue, Function<String, T> parser, Predicate<String> validator, FieldType type) {
        this(name, label, defaultValue, parser, validator, type, Collections.emptyMap(), Collections.emptyMap());
    }

    protected FormField(String name, String label, T defaultValue, Function<String, T> parser, Predicate<String> validator, FieldType fieldType, Map<String, String> options, Map<String, List<FormField<?>>> conditionalChildren) {
        this.name = name;
        this.label = label;
        this.defaultValue = defaultValue;
        this.parser = parser;
        this.validator = validator;
        this.fieldType = fieldType;

        this.options = options != null ? Collections.unmodifiableMap(new LinkedHashMap<>(options)) : Collections.emptyMap();
        this.conditionalChildren = conditionalChildren != null ? Collections.unmodifiableMap(new HashMap<>(conditionalChildren)) : Collections.emptyMap();
    }

    // --- Static Factory Methods ---

    public static FormField<String> text(String name, String label, String defaultValue) {
        return new FormField<>(name, label, defaultValue, s -> s, s -> !s.isBlank(), FieldType.TEXT, Collections.emptyMap(), Collections.emptyMap());
    }

    public static FormField<String> freeText(String name, String label, String defaultValue) {
        return new FormField<>(name, label, defaultValue, s -> s, s -> true, FieldType.TEXT, Collections.emptyMap(), Collections.emptyMap());
    }

    public static FormField<Integer> integer(String name, String label, Integer defaultValue) {
        return new FormField<>(name, label, defaultValue, Integer::parseInt, s -> s.matches("\\d+"), FieldType.NUMBER, Collections.emptyMap(), Collections.emptyMap());
    }

    public static FormField<String> cpf(String name, String label, String defaultValue) {
        return new FormField<>(name, label, defaultValue,
                s -> {
                    if (s == null || s.isBlank()) return null;

                    String cleaned = s.replaceAll("[^0-9]", "");

                    if (cleaned.length() != 11) throw new IllegalArgumentException("Invalid CPF length");
                    if (!cleaned.matches("\\d{11}")) throw new IllegalArgumentException("Invalid CPF format");

                    return cleaned;
                },
                s -> {
                    String cleaned = s.replaceAll("[^0-9]", "");

                    return cleaned.length() == 11 && cleaned.matches("\\d{11}");
                },
                FieldType.CPF, Collections.emptyMap(), Collections.emptyMap()
        );
    }

    public static <T> FormField<T> custom(String name, String label, T defaultValue, Function<String, T> parser, Predicate<String> validator, FieldType assumedType) {
        return new FormField<>(name, label, defaultValue, parser, validator, assumedType, Collections.emptyMap(), Collections.emptyMap());
    }

    public static FormField<LocalDate> date(String name, String label, LocalDate defaultValue) {
        return new FormField<>(
                name, label, defaultValue,
                s -> LocalDate.parse(s, GlobalScope.DATE_FORMAT), // Parser expects GlobalScope format
                s -> { // Validator
                    try { LocalDate.parse(s, GlobalScope.DATE_FORMAT); return true; }
                    catch (DateTimeParseException e) { return false; }
                },
                FieldType.DATE, Collections.emptyMap(), Collections.emptyMap()
        );
    }

    public static FormField<LocalDateTime> dateTime(String name, String label, LocalDateTime defaultValue) {
        return new FormField<>(
                name, label, defaultValue,
                s -> LocalDateTime.parse(s, GlobalScope.DATE_TIME_FORMAT), // Parser expects GlobalScope format
                s -> { // Validator
                    try { LocalDateTime.parse(s, GlobalScope.DATE_TIME_FORMAT); return true; }
                    catch (DateTimeParseException e) { return false; }
                },
                FieldType.DATETIME, Collections.emptyMap(), Collections.emptyMap()
        );
    }

    public static <E extends Enum<E>> FormField<E> choice(String name, String label, E defaultValue, Class<E> enumClass, Function<E, String> enumLabelProvider) {
        return choice(name, label, defaultValue, enumClass, enumLabelProvider, Collections.emptyMap());
    }

    public static <E extends Enum<E>> FormField<E> choice(String name, String label, E defaultValue, Class<E> enumClass, Function<E, String> enumLabelProvider, Map<String, List<FormField<?>>> conditionalChildren) {
        Map<String, String> opts = new LinkedHashMap<>();
        for (E enumConstant : enumClass.getEnumConstants()) {
            opts.put(enumConstant.name(), enumLabelProvider.apply(enumConstant));
        }
        Function<String, E> parser = s -> Enum.valueOf(enumClass, s);
        Predicate<String> validator = s -> {
            try {
                Enum.valueOf(enumClass, s);
                return true;
            } catch (IllegalArgumentException ex) {
                return false;
            }
        };
        return new FormField<>(name, label, defaultValue, parser, validator, FieldType.CHOICE, opts, conditionalChildren);
    }

    public static FormField<Boolean> confirmation(String name, String label, boolean defaultValue) {
        Map<String, String> opts = new LinkedHashMap<>();

        opts.put(Boolean.TRUE.toString(), "Yes");
        opts.put(Boolean.FALSE.toString(), "No");

        Function<String, Boolean> parser = s -> {
            if ("yes".equalsIgnoreCase(s) || Boolean.TRUE.toString().equalsIgnoreCase(s)) return true;
            if ("no".equalsIgnoreCase(s) || Boolean.FALSE.toString().equalsIgnoreCase(s)) return false;
            throw new IllegalArgumentException("Invalid boolean string: " + s + ". Expected 'yes', 'no', 'true', or 'false'.");
        };

        Predicate<String> validator = s -> "yes".equalsIgnoreCase(s) || "no".equalsIgnoreCase(s) ||
                Boolean.TRUE.toString().equalsIgnoreCase(s) ||
                Boolean.FALSE.toString().equalsIgnoreCase(s);

        return new FormField<>(name, label, defaultValue, parser, validator, FieldType.BOOLEAN, opts, Collections.emptyMap());
    }
}