package br.edu.ifba.entities.events;

import br.edu.ifba.entities.Event;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;

@Data
@EqualsAndHashCode(callSuper = true)
@SuperBuilder
public class Workshop extends Event {

    private boolean materialsProvided;
    private int numberOfSessions;

}
