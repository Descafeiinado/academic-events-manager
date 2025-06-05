package br.edu.ifba.aem.infrastructure.managers;

import br.edu.ifba.aem.application.GlobalScope;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.Type;

public class PersistenceManager {

  public static final PersistenceManager INSTANCE = new PersistenceManager();
  private static final String BASE_PATH = "data/";
  private final Gson gson;

  private PersistenceManager() {
    this.gson = GlobalScope.GSON;
  }

  public <T> T load(String fileName, Class<T> clazz) {
    ensureFileExists();

    try (FileReader reader = new FileReader(BASE_PATH + fileName)) {
      return gson.fromJson(reader, clazz);
    } catch (IOException exception) {
      System.err.println("Failed to load " + fileName + ": " + exception.getMessage());
      return null;
    }
  }

  public <T> T load(String fileName, Type type) {
    ensureFileExists();

    try (FileReader reader = new FileReader(BASE_PATH + fileName)) {
      return gson.fromJson(reader, type);
    } catch (IOException exception) {
      System.err.println("Failed to load " + fileName + ": " + exception.getMessage());
      return null;
    }
  }

  public void save(String fileName, JsonElement jsonElement) {
    ensureFileExists();

    try (FileWriter writer = new FileWriter(BASE_PATH + fileName)) {
      gson.toJson(jsonElement, writer);
    } catch (IOException exception) {
      System.err.println("Failed to save " + fileName + ": " + exception.getMessage());
    }
  }

  private void ensureFileExists() {
    File directory = new File(BASE_PATH);

    if (!directory.exists()) {
      if (!directory.mkdirs()) {
        System.err.println("Failed to create directory: " + BASE_PATH);
      }
    }
  }

}
