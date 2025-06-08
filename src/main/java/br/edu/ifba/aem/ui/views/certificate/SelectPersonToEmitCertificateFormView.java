package br.edu.ifba.aem.ui.views.certificate;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.infrastructure.services.CertificateService;
import br.edu.ifba.aem.infrastructure.services.pojos.FindCertifiableEventsPojo;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.components.PersonField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.utils.ConsoleColors;
import br.edu.ifba.aem.ui.views.PeopleManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class SelectPersonToEmitCertificateFormView extends FormPage implements View {

  public static final String NAME = "SELECT_PERSON_TO_EMIT_CERTIFICATE_FORM_VIEW";

  public SelectPersonToEmitCertificateFormView() {
    super("Select Person to Emit Certificate");
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new SelectPersonToEmitCertificateFormView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(PersonField.of("person", "Enter Person's CPF"));

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
      String personCpf = (String) results.get("person");

      FindCertifiableEventsPojo personToCertifiableEvents = CertificateService.INSTANCE.findCertifiableEvents(
          personCpf);

      if (!personToCertifiableEvents.events().isEmpty()) {
        Application.handleContextSwitch(
            new SelectEventToCertificateView(personToCertifiableEvents.person(),
                personToCertifiableEvents.events())
        );

        return;
      }
    } catch (Exception exception) {
      writer.println(
          ConsoleColors.RED_BACKGROUND + ConsoleColors.RED + "Failure:"
              + ConsoleColors.RESET + " " + exception.getMessage());

      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace(writer);
      }
    }

    promptReturnToLatestMenu(provider);
  }

  private void promptReturnToLatestMenu(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\nPress Enter to return to the People Management menu...");
    provider.readLine("");

    ViewRepository.INSTANCE.getById(PeopleManagementView.NAME)
        .ifPresent(Application::handleContextSwitch);
  }

}