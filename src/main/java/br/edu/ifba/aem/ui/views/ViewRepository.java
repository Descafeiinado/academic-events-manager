package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.infrastructure.repositories.core.IdToEntityRepository;
import br.edu.ifba.aem.ui.views.forms.AddPersonToEventFormView;
import br.edu.ifba.aem.ui.views.forms.EventCreationFormView;
import br.edu.ifba.aem.ui.views.forms.PersonCreationFormView;

public class ViewRepository extends IdToEntityRepository<String, View> {

  public static ViewRepository INSTANCE = new ViewRepository();

  public static void initialize() {
    MainView.initialize();
    EventManagementView.initialize();
    EventCreationFormView.initialize();

    PeopleManagementView.initialize();
    PersonCreationFormView.initialize();
    AddPersonToEventFormView.initialize();
  }

}
