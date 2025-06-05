package br.edu.ifba.aem.infrastructure.adapters;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import java.lang.reflect.Type;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class LocalDateAdapter implements JsonSerializer<LocalDate>, JsonDeserializer<LocalDate> {

  private static final DateTimeFormatter formatter = DateTimeFormatter.ISO_LOCAL_DATE;

  @Override
  public JsonElement serialize(LocalDate localDate, Type type, JsonSerializationContext context) {
    return new JsonPrimitive(localDate.format(formatter));
  }

  @Override
  public LocalDate deserialize(JsonElement jsonElement, Type type,
      JsonDeserializationContext context) throws JsonParseException {
    return LocalDate.parse(jsonElement.getAsString(), formatter);
  }
}
