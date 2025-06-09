package br.edu.ifba.aem.ui.views.certificate;

import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;
import br.edu.ifba.aem.ui.views.PeopleManagementView;
import br.edu.ifba.aem.ui.views.View;
import java.util.List;

public class SelectEventToCertificateView extends NavigationPage implements View {

  public static final String NAME = "SELECT_EVENT_TO_CERTIFICATE_VIEW";

  private final Person person;
  private final List<Event> certifiableEvents;

  public SelectEventToCertificateView(Person person, List<Event> certifiableEvents) {
    super("Select Event to Emit Certificate for " + person.getName());

    this.person = person;
    this.certifiableEvents = certifiableEvents;
  }

  public static void initialize() {
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    for (Event event : certifiableEvents) {
      String optionName = String.format("%s | %s: %s",
          event.getDate().format(GlobalScope.DATE_FORMAT),
          event.getType().getLabel(), event.getTitle());

      addCustomAction(optionName, () -> Application.handleContextSwitch(
          new ConfirmCertificateEmissionFormView(person, event)));
    }

    addBackOption(provider, PeopleManagementView.NAME);
  }

}