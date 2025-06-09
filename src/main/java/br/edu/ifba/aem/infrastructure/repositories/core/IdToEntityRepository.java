package br.edu.ifba.aem.infrastructure.repositories.core;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

@SuppressWarnings("unused")
public class IdToEntityRepository<I, T> {

  protected Map<I, T> data;

  public IdToEntityRepository() {
    this.data = new HashMap<>();
  }

  public Optional<T> getById(I id) {
    return Optional.ofNullable(data.get(id));
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

  public Stream<T> stream() {
    return data.values().stream();
  }

}