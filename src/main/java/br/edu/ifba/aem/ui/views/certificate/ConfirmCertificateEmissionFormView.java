package br.edu.ifba.aem.ui.views.certificate;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.utils.Pair;
import br.edu.ifba.aem.infrastructure.services.CertificateService;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.utils.ConsoleColors;
import br.edu.ifba.aem.ui.views.PeopleManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import lombok.Getter;

@Getter
public class ConfirmCertificateEmissionFormView extends FormPage implements View {

  public static final String NAME = "CONFIRM_CERTIFICATE_EMISSION_FORM_VIEW";

  private final Person person;
  private final Event event;

  public ConfirmCertificateEmissionFormView(Person person, Event event) {
    super("Confirm Certificate Emission for " + person.getName() + " - " + event.getTitle());

    this.person = person;
    this.event = event;
  }

  public static void initialize() {
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(FormField.confirmation("confirm",
        "Are you sure you want to issue a certificate for '" + person.getName() + "' in the event '"
            + event.getTitle() + "'?", true));

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
      boolean isEmissionConfirmed = Boolean.TRUE.equals(results.get("confirm"));

      if (!isEmissionConfirmed) {
        writer.println("Certificate emission cancelled by user.");

        promptReturnToLatestMenu(provider);
        return;
      }

      writer.println(ConsoleColors.GREEN_BOLD + "Generating certificate for " + person.getName()
          + " for event: " + event.getTitle() + ConsoleColors.RESET);

      Pair<String, String> result = CertificateService.INSTANCE.generateCertificate(person, event);

      if (result != null) {
        writer.println(
            ConsoleColors.GREEN_BOLD + "Certificate generated successfully: " + result.left()
                + ConsoleColors.RESET);
        writer.println("\n" + result.right());
      } else {
        throw new RuntimeException("Failed to generate certificate.");
      }
    } catch (Exception exception) {
      writer.println(
          ConsoleColors.RED_BACKGROUND + ConsoleColors.RED + "Failure:" + ConsoleColors.RESET + " "
              + exception.getMessage());

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