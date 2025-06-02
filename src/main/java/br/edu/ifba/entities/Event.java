package br.edu.ifba.entities;

import br.edu.ifba.entities.enums.EventModality;
import br.edu.ifba.entities.enums.EventType;
import br.edu.ifba.ui.components.form.FormField;
import lombok.Data;
import lombok.experimental.SuperBuilder;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@Data
@SuperBuilder
public abstract class Event {

    private static final SequentialIdentifierProvider SEQUENTIAL_ID_PROVIDER = new SequentialIdentifierProvider();

    private final Long id = SEQUENTIAL_ID_PROVIDER.getNextId();

    private String title;
    private LocalDateTime date;
    private String place;

    private EventType type;
    private EventModality modality;

    private List<String> inPersonParticipants = new ArrayList<>();
    private List<String> virtualParticipants = new ArrayList<>();

    private Integer capacity;
    private String description;

    public Event() {
        SEQUENTIAL_ID_PROVIDER.rollback();
    }

    public abstract List<FormField<?>> getSpecificFields();

    /*
        Static Methods
     */

    public static SequentialIdentifierProvider getSequentialIdProvider() {
        return SEQUENTIAL_ID_PROVIDER;
    }

}
