package br.edu.ifba.aem.domain.models;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public record DateRange(LocalDate start, LocalDate end) {

  public static final String DATE_RANGE_FORMAT = "dd/MM/yyyy-dd/MM/yyyy";
  public static final String DATE_RANGE_REGEX = "\\d{2}/\\d{2}/\\d{4}-\\d{2}/\\d{2}/\\d{4}";

  public DateRange {
    if (start.isAfter(end)) {
      throw new IllegalArgumentException("Start date must be before end date.");
    }
  }

  public static DateRange parse(String dateRange) {
    if (dateRange == null || !dateRange.matches(DATE_RANGE_REGEX)) {
      throw new IllegalArgumentException(
          "Invalid date range format. Expected format: " + DATE_RANGE_FORMAT);
    }

    String[] dates = dateRange.split("-");

    LocalDate start = LocalDate.parse(dates[0], DateTimeFormatter.ofPattern("dd/MM/yyyy"));
    LocalDate end = LocalDate.parse(dates[1], DateTimeFormatter.ofPattern("dd/MM/yyyy"));

    return new DateRange(start, end);
  }

  public boolean contains(LocalDate dateTime) {
    return (dateTime.equals(start) || dateTime.isAfter(start)) && (dateTime.equals(end)
        || dateTime.isBefore(end));
  }

  @Override
  public String toString() {
    return String.format("%s-%s", start.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")),
        end.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));
  }

}
