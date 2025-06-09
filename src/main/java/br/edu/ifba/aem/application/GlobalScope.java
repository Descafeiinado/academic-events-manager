package br.edu.ifba.aem.application;

import br.edu.ifba.aem.infrastructure.adapters.LocalDateAdapter;
import br.edu.ifba.aem.infrastructure.adapters.LocalDateTimeAdapter;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

public class GlobalScope {

  public static Gson GSON = new GsonBuilder()
      .setPrettyPrinting()
      .registerTypeAdapter(LocalDate.class, new LocalDateAdapter())
      .registerTypeAdapter(LocalDateTime.class, new LocalDateTimeAdapter())
      .create();

  public static String DATE_FORMAT_STRING = "dd/MM/yyyy";
  public static String DATE_TIME_FORMAT_STRING = "dd/MM/yyyy HH:mm";

  public static DateTimeFormatter DATE_FORMAT = DateTimeFormatter.ofPattern(DATE_FORMAT_STRING);
  public static DateTimeFormatter DATE_TIME_FORMAT = DateTimeFormatter.ofPattern(
      DATE_TIME_FORMAT_STRING);

  public static String CPF_REGEX = "(\\d{3})(\\d{3})(\\d{3})(\\d{2})";

  public static Function<String, String> CPF_FORMATTER = cpf -> cpf
      .replaceAll("\\D", "")
      .replaceFirst(CPF_REGEX, "$1.$2.$3-$4");
  public static Function<String, String> CPF_REDACTOR = cpf -> cpf
      .replaceAll("\\D", "")
      .replaceFirst(CPF_REGEX, "$1.***.***-$4");
}
