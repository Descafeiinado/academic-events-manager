package br.edu.ifba.aem.ui.views.events;

import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.infrastructure.repositories.impl.PersonRepository;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.pages.types.StaticPage;
import br.edu.ifba.aem.ui.utils.ConsoleColors;
import br.edu.ifba.aem.ui.views.View;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

public class EventDetailsView extends StaticPage implements View {

  public static final String NAME = "EVENT_DETAILS_VIEW";

  private final Event event;

  public EventDetailsView(Event event, View returnView) {
    this(event,
        returnView != null ? onGoBack -> Application.handleContextSwitch(returnView) : null);
  }

  public EventDetailsView(Event event, Consumer<InteractionProvider> onGoBack) {
    super("Event Details for: " + event.getTitle());

    this.event = event;
    this.getEventDetails().forEach(this::addText);
    this.setOnGoBack(onGoBack);
  }

  public static void initialize() {
  }

  private List<String> getEventDetails() {
    List<String> lines = new ArrayList<>();

    lines.add(formatField("ID", "#" + event.getId()));
    lines.add(formatField("Event", event.getTitle()));
    lines.add(formatField("Type", event.getType() != null ? event.getType().getLabel() : "N/A"));
    lines.add(formatField("When", GlobalScope.DATE_TIME_FORMAT.format(event.getDate())));
    lines.add(formatField("Modality",
        event.getModality() != null ? event.getModality().getLabel() : "N/A"));
    lines.add(formatField("Place", event.getPlace() != null ? event.getPlace() : "N/A"));
    lines.add(formatField("Description",
        event.getDescription() != null ? event.getDescription() : "N/A"));

    lines.add(formatField("Created At", GlobalScope.DATE_TIME_FORMAT.format(event.getCreatedAt())));

    lines.add("");

    event.getDescriptiveFields().forEach(pair -> lines.add(formatField(pair.left(), pair.right())));

    lines.add("\n=== PARTICIPANTS FOR EVENT: " + event.getTitle().toUpperCase() + " (Total: "
        + event.getAllParticipants().size() + ") ===\n");

    if (event.getAllParticipants().isEmpty()) {
      lines.add("No participants registered for this event.");
    } else {
      event.getAllParticipants().forEach(participant -> {
        Person person = PersonRepository.INSTANCE.getById(participant).orElse(null);

        if (person == null) {
          lines.add(" • " + ConsoleColors.RED_BOLD + "Unknown Participant with ID: " + participant
              + ConsoleColors.RESET);
          return;
        }

        String participantInfo =
            person.getName() + " (" + GlobalScope.CPF_REDACTOR.apply(person.getCpf()) + ")";

        lines.add(" • " + ConsoleColors.YELLOW_BOLD + participantInfo + ConsoleColors.RESET);
      });
    }

    return lines;
  }

  private String formatField(String context, String value) {
    return " • " + ConsoleColors.GREEN_BOLD + context + ": " + ConsoleColors.RESET
        + ConsoleColors.WHITE + value + ConsoleColors.RESET;
  }

  @Override
  public String getName() {
    return NAME;
  }

}
