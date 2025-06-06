package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.TextComponent;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;
import br.edu.ifba.aem.ui.views.forms.AddPersonToEventFormView;
import br.edu.ifba.aem.ui.views.forms.PersonCreationFormView;

public class PeopleManagementView extends NavigationPage implements View {

  public static final String NAME = "PEOPLE_MANAGEMENT_VIEW";

  public PeopleManagementView() {
    super("People Management");

    addComponent(new TextComponent("You can manage people here."));
    addComponent(new TextComponent("Please choose an option below:"));
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new PeopleManagementView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    addNavigationOption("Create New Person", PersonCreationFormView.NAME);
    addNavigationOption("Add Person to Event", AddPersonToEventFormView.NAME);

    addBackOption(provider, "MAIN");
  }
}