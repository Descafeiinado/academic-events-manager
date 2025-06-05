package br.edu.ifba.aem.ui.components;

import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.infrastructure.repositories.impl.EventRepository;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import lombok.Getter;
import lombok.Setter;

@Getter

public class EventField extends FormField<Long> {

  private final Function<String, Optional<Event>> displayEventRetriever;
  @Setter private transient String resolvedConditionalKey = null;

  public EventField(String name, String label, Function<Event, Boolean> eventBusinessValidator) {
    super(
        name,
        label,
        null,
        createEventParser(eventBusinessValidator),
        createEventValidator(eventBusinessValidator),
        FieldType.EVENT,
        Collections.emptyMap(),
        Collections.emptyMap()
    );

    this.displayEventRetriever = createInternalDisplayEventRetriever();
  }

  public EventField withConditionalChildren(Map<String, List<FormField<?>>> conditionalChildren) {
    return new EventField(
        this.getName(),
        this.getLabel(),
        event -> true
    ) {
      @Override
      public Map<String, List<FormField<?>>> getConditionalChildren() {
        return conditionalChildren;
      }
    };
  }

  private static Function<String, Long> createEventParser(
      Function<Event, Boolean> eventBusinessValidator) {
    return idInput -> {
      if (idInput == null || idInput.isBlank()) {
        throw new IllegalArgumentException("Event ID cannot be blank.");
      }

      String trimmedIdInput = idInput.trim();
      long parsedId;

      try {
        parsedId = Long.parseLong(trimmedIdInput);
      } catch (NumberFormatException e) {
        throw new IllegalArgumentException(
            "Invalid Event ID format: '" + trimmedIdInput + "'. Must be a number.", e);
      }

      EventRepository eventRepository = EventRepository.INSTANCE;
      Optional<Event> eventOptional = eventRepository.getById(parsedId);

      if (eventOptional.isEmpty()) {
        throw new IllegalArgumentException("Event not found for ID: " + parsedId);
      }

      Event event = eventOptional.get();

      if (eventBusinessValidator != null) {
        try {
          if (!eventBusinessValidator.apply(event)) {
            throw new IllegalArgumentException(
                "Event failed business validation: " + event.getTitle() + " (ID: " + parsedId
                    + ")");
          }
        } catch (IllegalArgumentException exception) {
          throw exception;
        } catch (Exception exception) {
          throw new IllegalArgumentException(
              "Error during event business validation for Event ID " + parsedId + ": "
                  + exception.getMessage(), exception);
        }
      }
      return parsedId;
    };
  }

  private static Predicate<String> createEventValidator(
      Function<Event, Boolean> eventBusinessValidator) {
    return idInput -> {
      if (idInput == null || idInput.isBlank()) {
        return false;
      }

      String trimmedIdInput = idInput.trim();
      long parsedId;

      try {
        parsedId = Long.parseLong(trimmedIdInput);
      } catch (NumberFormatException e) {
        return false;
      }

      EventRepository eventRepository = EventRepository.INSTANCE;
      Optional<Event> eventOptional = eventRepository.getById(parsedId);

      if (eventOptional.isEmpty()) {
        return false;
      }

      if (eventBusinessValidator != null) {
        try {
          return eventBusinessValidator.apply(eventOptional.get());
        } catch (Exception exception) {
          return false;
        }
      }

      return true;
    };
  }

  private static Function<String, Optional<Event>> createInternalDisplayEventRetriever() {
    return parsedId -> {
      if (parsedId == null || parsedId.isBlank()) {
        return Optional.empty();
      }

      String trimmedIdInput = parsedId.trim();

      EventRepository eventRepository = EventRepository.INSTANCE;

      try {
        return eventRepository.getById(Long.parseLong(trimmedIdInput));
      } catch (NumberFormatException e) {
        return Optional.empty();
      }
    };
  }

  public static EventField of(String name, String label) {
    return new EventField(name, label, null);
  }

  public static EventField of(String name, String label,
      Function<Event, Boolean> eventBusinessValidator) {
    return new EventField(name, label, eventBusinessValidator);
  }

  public Function<String, Optional<Event>> getDisplayEventRetrieverFunction() {
    return this.displayEventRetriever;
  }
}