package br.edu.ifba.repositories.impl;

import br.edu.ifba.GlobalScope;
import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.Person;
import br.edu.ifba.entities.enums.EventType;
import br.edu.ifba.entities.enums.PersonType;
import br.edu.ifba.managers.PersistenceManager;
import br.edu.ifba.repositories.IdToEntityRepository;
import br.edu.ifba.repositories.PersistentRepository;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;

public class PersonRepository extends IdToEntityRepository<String, Person> implements PersistentRepository {

    public static final PersonRepository INSTANCE = new PersonRepository();

    @Override
    public void load() {
        Type jsonArrayType = new TypeToken<JsonArray>(){}.getType();
        JsonArray persons = PersistenceManager.INSTANCE.load(getFileName(), jsonArrayType);

        if (persons == null) return;

        for (int i = 0; i < persons.size(); i++) {
            JsonObject rawPerson = GlobalScope.GSON.fromJson(persons.get(i), JsonObject.class);

            PersonType personType = PersonType.fromName(rawPerson.get("type").getAsString());
            Person person = GlobalScope.GSON.fromJson(rawPerson, personType.getEventClass());

            System.out.printf("Parsed person: %s%n", person);

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
        System.out.println(jsonArray);

        PersistenceManager.INSTANCE.save(getFileName(), jsonArray);
    }

}
