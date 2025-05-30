package br.edu.ifba.repositories;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public abstract class AbstractIdToEntityRepository<I, T> {

    protected Map<I, T> data;

    public AbstractIdToEntityRepository() {
        this.data = new HashMap<>();
    }

    public Optional<T> getById(I id) {
        return Optional.of(data.get(id));
    }

    public boolean exists(I id) {
        return data.containsKey(id);
    }

    public void deleteById(I id) {
        data.remove(id);
    }

    public Collection<T> getAll() {
        return data.values();
    }

    public T save(I id, T value) {
        return data.put(id, value);
    }

}