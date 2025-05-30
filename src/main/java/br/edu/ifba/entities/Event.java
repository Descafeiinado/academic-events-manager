package br.edu.ifba.entities;

import br.edu.ifba.entities.enums.EventModality;
import br.edu.ifba.entities.enums.EventType;
import lombok.Data;
import lombok.experimental.SuperBuilder;

import java.util.List;

@Data
@SuperBuilder
public class Event {

    private static final SequentialIdentifierProvider SEQUENTIAL_ID_PROVIDER = new SequentialIdentifierProvider();

    private final Long id = SEQUENTIAL_ID_PROVIDER.getNextId();

    private String title;
    private String date;
    private String place;

    private EventType type;
    private EventModality modality;

    private List<Long> inPersonParticipants;
    private List<Long> virtualParticipants;

    private Integer capacity;
    private String description;

    /*
        Static Methods
     */

    public static SequentialIdentifierProvider getSequentialIdProvider() {
        return SEQUENTIAL_ID_PROVIDER;
    }

}
