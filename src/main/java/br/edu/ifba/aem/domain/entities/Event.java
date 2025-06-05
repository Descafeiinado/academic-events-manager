package br.edu.ifba.aem.domain.entities;

import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.ui.components.FormField;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.SuperBuilder;

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

  @Builder.Default
  private List<String> inPersonParticipants = new ArrayList<>();
  @Builder.Default
  private List<String> virtualParticipants = new ArrayList<>();

  private Integer capacity;
  private String description;

  public Event() {
    SEQUENTIAL_ID_PROVIDER.rollback();
  }

  public static SequentialIdentifierProvider getSequentialIdProvider() {
    return SEQUENTIAL_ID_PROVIDER;
  }

    /*
        Static Methods
     */

  public abstract List<FormField<?>> getSpecificFields();

}
