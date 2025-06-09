package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.TextComponent;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;
import br.edu.ifba.aem.ui.views.reports.ReportEventsByDateFormView;
import br.edu.ifba.aem.ui.views.reports.ReportEventsByDateRangeFormView;
import br.edu.ifba.aem.ui.views.reports.ReportEventsByEventTypeFormView;

public class ReportGenerationView extends NavigationPage implements View {

  public static final String NAME = "REPORT_GENERATION_VIEW";

  public ReportGenerationView() {
    super("Report Generation");

    addComponent(new TextComponent("You can generate reports here."));
    addComponent(new TextComponent("Please choose an option below:"));
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new ReportGenerationView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    addNavigationOption("Report Events by Date", ReportEventsByDateFormView.NAME);
    addNavigationOption("Report Events by Date Range", ReportEventsByDateRangeFormView.NAME);
    addNavigationOption("Report Events by Type", ReportEventsByEventTypeFormView.NAME);

    addBackOption(provider, "MAIN");
  }
}