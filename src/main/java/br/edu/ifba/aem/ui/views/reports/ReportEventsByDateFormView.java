package br.edu.ifba.aem.ui.views.reports;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.domain.entities.Event;
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
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ReportEventsByDateFormView extends FormPage implements View {

  public static final String NAME = "REPORT_EVENTS_BY_DATE_FORM_VIEW";

  public ReportEventsByDateFormView() {
    super("Set Date for Event Report");
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new ReportEventsByDateFormView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(FormField.date("date", "Enter Start Date", LocalDate.now()));

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
      LocalDate date = (LocalDate) results.get("date");

      List<Event> events = EventService.INSTANCE.findEventsByDate(date);

      if (!events.isEmpty()) {
        Application.handleContextSwitch(new EventListView("Events Report for " + date, events,
            event -> String.format("%s: %s", event.getType().getLabel(), event.getTitle()),
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