package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.TextComponent;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;

public class MainView extends NavigationPage implements View {

  public static final String NAME = "MAIN";

  public MainView() {
    super("Main Menu");

    addComponent(new TextComponent("Welcome to the Event Management System!"));
    addComponent(new TextComponent("Please choose an option below:"));
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new MainView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    addNavigationOption("Manage Events", EventManagementView.NAME);
    addNavigationOption("Manage People", PeopleManagementView.NAME);
    addNavigationOption("Generate Reports", ReportGenerationView.NAME);

    addExitOption(provider);
  }
}