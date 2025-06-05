package br.edu.ifba.aem.infrastructure.repositories.impl;

import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.enums.PersonType;
import br.edu.ifba.aem.infrastructure.managers.PersistenceManager;
import br.edu.ifba.aem.infrastructure.repositories.core.IdToEntityRepository;
import br.edu.ifba.aem.infrastructure.repositories.core.PersistentRepository;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;
import java.lang.reflect.Type;

public class PersonRepository extends IdToEntityRepository<String, Person> implements
    PersistentRepository {

  public static final PersonRepository INSTANCE = new PersonRepository();

  private PersonRepository() {}

  @Override
  public void load() {
    Type jsonArrayType = new TypeToken<JsonArray>() {
    }.getType();
    JsonArray persons = PersistenceManager.INSTANCE.load(getFileName(), jsonArrayType);

    if (persons == null) {
      return;
    }

    for (int i = 0; i < persons.size(); i++) {
      JsonObject rawPerson = GlobalScope.GSON.fromJson(persons.get(i), JsonObject.class);

      PersonType personType = PersonType.fromName(rawPerson.get("type").getAsString());
      Person person = GlobalScope.GSON.fromJson(rawPerson, personType.getEventClass());

      save(person.getCpf(), person);
    }
  }

  @Override
  public void persist() {
    JsonArray jsonArray = new JsonArray();

    for (Person person : getAll()) {
      jsonArray.add(GlobalScope.GSON.toJsonTree(person, person.getType().getEventClass()));
    }

    System.out.printf("Saving %d persons to JSON...%n", jsonArray.size());

    PersistenceManager.INSTANCE.save(getFileName(), jsonArray);
  }

}
