package br.edu.ifba.aem.ui.views.reports;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.enums.EventType;
import br.edu.ifba.aem.infrastructure.services.EventService;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.utils.ConsoleColors;
import br.edu.ifba.aem.ui.views.ReportGenerationView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import br.edu.ifba.aem.ui.views.events.EventListView;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ReportEventsByEventTypeFormView extends FormPage implements View {

  public static final String NAME = "REPORT_EVENTS_BY_EVENT_TYPE_FORM_VIEW";

  public ReportEventsByEventTypeFormView() {
    super("Select Event Type for Report");
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new ReportEventsByEventTypeFormView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(FormField.choice("type", "Select event type", EventType.LECTURE, EventType.class,
        EventType::getLabel));

    return fields;
  }

  @Override
  public void onInterruptScreen(InteractionProvider provider) {
  }

  @Override
  protected void onSubmit(InteractionProvider provider, Map<String, Object> results) {
    PrintWriter writer = provider.getWriter();
    writer.println("\n--- Form Submission Processing ---");

    boolean hasNoData = results == null || results.isEmpty();

    if (hasNoData) {
      writer.println("Form was cancelled, an error occurred, or no data was entered.");

      promptReturnToLatestMenu(provider);
      return;
    }

    try {
      EventType range = (EventType) results.get("type");

      List<Event> events = EventService.INSTANCE.findEventsByType(range);

      if (!events.isEmpty()) {
        Application.handleContextSwitch(new EventListView("Events Report for " + range, events,
            event -> String.format("%s | %s", event.getDate().format(GlobalScope.DATE_FORMAT),
                event.getTitle()),
            ViewRepository.INSTANCE.getById(ReportGenerationView.NAME).orElseThrow(
                () -> new IllegalStateException(
                    "ReportGenerationView not found in ViewRepository."))));

        return;
      }
    } catch (Exception exception) {
      writer.println(ConsoleColors.RED_BACKGROUND + ConsoleColors.RED + "Failed to achieve report:"
          + ConsoleColors.RESET + " " + exception.getMessage());

      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace(writer);
      }
    }

    promptReturnToLatestMenu(provider);
  }

  private void promptReturnToLatestMenu(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\nPress Enter to return to the Report Generation menu...");
    provider.readLine("");

    ViewRepository.INSTANCE.getById(ReportGenerationView.NAME)
        .ifPresent(Application::handleContextSwitch);
  }

}