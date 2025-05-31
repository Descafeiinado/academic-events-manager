package br.edu.ifba.entities;

import lombok.Data;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@SuperBuilder
public abstract class Person {

    private static final SequentialIdentifierProvider SEQUENTIAL_ID_PROVIDER = new SequentialIdentifierProvider();

    private final Long id = SEQUENTIAL_ID_PROVIDER.getNextId();

    private String cpf;
    private String name;
    private LocalDateTime birthday;

    private List<Long> eventsParticipated = new ArrayList<>();

    public Person() {
        SEQUENTIAL_ID_PROVIDER.rollback();
    }

    /*
        Static Methods
     */

    public static SequentialIdentifierProvider getSequentialIdProvider() {
        return SEQUENTIAL_ID_PROVIDER;
    }

}
