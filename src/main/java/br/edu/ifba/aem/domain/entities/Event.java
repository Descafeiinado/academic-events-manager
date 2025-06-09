package br.edu.ifba.aem.domain.entities;

import br.edu.ifba.aem.domain.enums.EventModality;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.domain.models.EventCapacity;
import br.edu.ifba.aem.domain.utils.Pair;
import br.edu.ifba.aem.ui.components.FormField;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
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
  private String description;

  private EventType type;
  private EventModality modality;
  private EventCapacity capacity;

  private LocalDateTime createdAt = LocalDateTime.now();

  private List<String> inPersonParticipants = new ArrayList<>();
  private List<String> virtualParticipants = new ArrayList<>();

  public Event() {
    SEQUENTIAL_ID_PROVIDER.rollback();
  }

  public Set<String> getAllParticipants() {
    Set<String> allParticipants = new HashSet<>();

    allParticipants.addAll(inPersonParticipants);
    allParticipants.addAll(virtualParticipants);

    return allParticipants;
  }

  public static SequentialIdentifierProvider getSequentialIdProvider() {
    return SEQUENTIAL_ID_PROVIDER;
  }

   /*
      Static Methods
   */

  public abstract String getCertificateTemplate(Person person);

  public abstract List<FormField<?>> getSpecificFields();

  public abstract List<Pair<String, String>> getDescriptiveFields();

}
