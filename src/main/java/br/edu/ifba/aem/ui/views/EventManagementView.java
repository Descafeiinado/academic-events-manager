package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.TextComponent;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;
import br.edu.ifba.aem.ui.views.forms.EventCreationFormView;

public class EventManagementView extends NavigationPage implements View {

  public static final String NAME = "EVENT_MANAGEMENT_VIEW";

  public EventManagementView() {
    super("Event Management");

    addComponent(new TextComponent("You can manage events here."));
    addComponent(new TextComponent("Please choose an option below:"));
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new EventManagementView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    addNavigationOption("Create New Event", EventCreationFormView.NAME);

    addBackOption(provider, "MAIN");
  }
}