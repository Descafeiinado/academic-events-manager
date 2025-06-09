package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.infrastructure.repositories.core.IdToEntityRepository;
import br.edu.ifba.aem.ui.views.certificate.ConfirmCertificateEmissionFormView;
import br.edu.ifba.aem.ui.views.certificate.SelectEventToCertificateView;
import br.edu.ifba.aem.ui.views.certificate.SelectPersonToEmitCertificateFormView;
import br.edu.ifba.aem.ui.views.events.EventListView;
import br.edu.ifba.aem.ui.views.forms.AddPersonToEventFormView;
import br.edu.ifba.aem.ui.views.forms.EventCreationFormView;
import br.edu.ifba.aem.ui.views.forms.PersonCreationFormView;
import br.edu.ifba.aem.ui.views.reports.ReportEventsByDateFormView;
import br.edu.ifba.aem.ui.views.reports.ReportEventsByDateRangeFormView;
import br.edu.ifba.aem.ui.views.reports.ReportEventsByEventTypeFormView;

public class ViewRepository extends IdToEntityRepository<String, View> {

  public static ViewRepository INSTANCE = new ViewRepository();

  public static void initialize() {
    MainView.initialize();

    EventManagementView.initialize();
    EventCreationFormView.initialize();
    EventListView.initialize();

    PeopleManagementView.initialize();
    PersonCreationFormView.initialize();
    AddPersonToEventFormView.initialize();

    SelectPersonToEmitCertificateFormView.initialize();
    SelectEventToCertificateView.initialize();
    ConfirmCertificateEmissionFormView.initialize();

    ReportGenerationView.initialize();
    ReportEventsByDateFormView.initialize();
    ReportEventsByDateRangeFormView.initialize();
    ReportEventsByEventTypeFormView.initialize();

  }

}
