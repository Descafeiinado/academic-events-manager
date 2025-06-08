package br.edu.ifba.aem.domain.entities;

import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.domain.models.EventCapacity;
import br.edu.ifba.aem.ui.components.FormField;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
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
  private EventCapacity capacity;

  private List<String> inPersonParticipants = new ArrayList<>();
  private List<String> virtualParticipants = new ArrayList<>();

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

  public abstract String getCertificateTemplate(Person person);

  public abstract List<FormField<?>> getSpecificFields();

}
