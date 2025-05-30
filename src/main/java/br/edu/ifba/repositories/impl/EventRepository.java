package br.edu.ifba.repositories.impl;

import br.edu.ifba.entities.Event;
import br.edu.ifba.repositories.IdToEntityRepository;
import br.edu.ifba.repositories.PersistentRepository;

public class EventRepository extends IdToEntityRepository<Long, Event> implements PersistentRepository {

    public static final EventRepository INSTANCE = new EventRepository();

    @Override
    public void load() {
        // TODO
    }

    @Override
    public void persist() {
        // TODO
    }
}
