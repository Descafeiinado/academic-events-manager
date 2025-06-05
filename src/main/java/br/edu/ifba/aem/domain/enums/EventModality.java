package br.edu.ifba.aem.domain.enums;

import java.util.Arrays;
import java.util.stream.Stream;
import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public enum EventModality {
  IN_PERSON("In-person"),
  VIRTUAL("Virtual"),
  HYBRID("Hybrid");

  private final String label;

  public static Stream<EventModality> stream() {
    return Arrays.stream(EventModality.values());
  }

  @Override
  public String toString() {
    return label;
  }
}
