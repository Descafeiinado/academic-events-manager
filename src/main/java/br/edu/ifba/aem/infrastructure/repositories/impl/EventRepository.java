package br.edu.ifba.aem.infrastructure.repositories.impl;

import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.infrastructure.managers.PersistenceManager;
import br.edu.ifba.aem.infrastructure.repositories.core.IdToEntityRepository;
import br.edu.ifba.aem.infrastructure.repositories.core.PersistentRepository;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;

public class EventRepository extends IdToEntityRepository<Long, Event> implements
    PersistentRepository {

  public static final EventRepository INSTANCE = new EventRepository();

  @Override
  public Event save(Long id, Event value) {
    super.save(id, value);
    this.persist();

    return value;
  }

  @Override
  public void load() {
    Type jsonArrayType = new TypeToken<JsonArray>() {
    }.getType();
    JsonArray events = PersistenceManager.INSTANCE.load(getFileName(), jsonArrayType);

    if (events == null) {
      return;
    }

    Long maxId = 0L;

    for (int i = 0; i < events.size(); i++) {
      JsonObject rawEvent = GlobalScope.GSON.fromJson(events.get(i), JsonObject.class);

      EventType eventType = EventType.fromName(rawEvent.get("type").getAsString());
      Event event = GlobalScope.GSON.fromJson(rawEvent, eventType.getEventClass());

      save(event.getId(), event);

      if (event.getId() > maxId) {
        maxId = event.getId();
      }
    }

    Event.getSequentialIdProvider().setCurrentId(maxId);
  }

  @Override
  public void persist() {
    JsonArray jsonArray = new JsonArray();

    for (Event event : getAll()) {
      jsonArray.add(GlobalScope.GSON.toJsonTree(event, event.getType().getEventClass()));
    }

    System.out.printf("Saving %d events to JSON...%n", jsonArray.size());

    PersistenceManager.INSTANCE.save(getFileName(), jsonArray);
  }

}
