package br.edu.ifba.aem.ui.views.forms;

import br.edu.ifba.aem.application.AppConfig;
import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.domain.entities.Person;
import br.edu.ifba.aem.domain.enums.PersonType;
import br.edu.ifba.aem.infrastructure.services.PersonService;
import br.edu.ifba.aem.infrastructure.services.pojos.PersonCreationRequestPojo;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.FormField;
import br.edu.ifba.aem.ui.pages.types.FormPage;
import br.edu.ifba.aem.ui.views.PeopleManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class PersonCreationFormView extends FormPage implements View {

  public static final String NAME = "NEW_PERSON_FORM_VIEW";

  public PersonCreationFormView() {
    super("Create New Person");
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new PersonCreationFormView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  protected List<FormField<?>> getFormFields() {
    List<FormField<?>> fields = new ArrayList<>();

    fields.add(FormField.cpf("cpf", "Enter Person's CPF", null));
    fields.add(FormField.text("name", "Enter Person's Name", "John Doe"));
    fields.add(FormField.date("birthDate", "Enter Person's Birth Date",
        LocalDateTime.now().minusYears(20).toLocalDate()));

    fields.add(FormField.choice("category", "Select Person's Category", PersonType.EXTERNAL,
        PersonType.class, PersonType::getLabel));

    fields.add(
        FormField.confirmation("confirmCreation", "Are you sure you want to create this person?",
            true));

    return fields;
  }

  @Override
  public void onInterruptScreen(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\n--- Person Creation Interrupted ---");
    writer.println("Person creation form was interrupted or cancelled.");
    writer.println("Returning to the people management menu...");

    ViewRepository.INSTANCE.getById(PeopleManagementView.NAME)
        .ifPresent(Application::handleContextSwitch);
  }

  @Override
  protected void onSubmit(InteractionProvider provider, Map<String, Object> results) {
    PrintWriter writer = provider.getWriter();
    writer.println("\n--- Form Submission Processing ---");

    boolean hasNoData = results == null || results.isEmpty();
    boolean isCreationConfirmed = !hasNoData && Boolean.TRUE.equals(results.get("confirmCreation"));

    if (!isCreationConfirmed) {
      if (hasNoData) {
        writer.println("Form was cancelled, an error occurred, or no data was entered.");
      } else {
        writer.println("Person creation cancelled by user.");
      }

      promptReturnToLatestMenu(provider);
      return;
    }

    try {
      Person person = PersonService.INSTANCE.createPerson(buildRequestPojo(results));

      writer.println(
          String.format("\nPerson '%s' with CPF %s created successfully!", person.getName(),
              GlobalScope.CPF_FORMATTER.apply(person.getCpf())));
    } catch (Exception exception) {
      writer.println("Error creating person: " + exception.getMessage());

      if (AppConfig.DEBUG_MODE) {
        exception.printStackTrace(writer);
      }
    }

    promptReturnToLatestMenu(provider);
  }

  protected PersonCreationRequestPojo buildRequestPojo(Map<String, Object> results) {
    PersonType selectedPersonType = (PersonType) results.get("category");

    if (selectedPersonType == null) {
      throw new IllegalArgumentException("Person type cannot be null");
    }

    return new PersonCreationRequestPojo((String) results.get("cpf"), (String) results.get("name"),
        (LocalDate) results.get("birthDate"), selectedPersonType);
  }

  private void promptReturnToLatestMenu(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println("\nPress Enter to return to the People Management menu...");
    provider.readLine("");

    ViewRepository.INSTANCE.getById(PeopleManagementView.NAME)
        .ifPresent(Application::handleContextSwitch);
  }

}