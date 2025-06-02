package br.edu.ifba.views.impl.forms;

import br.edu.ifba.GlobalScope;
import br.edu.ifba.applications.Application;
import br.edu.ifba.entities.Event;
import br.edu.ifba.entities.Person;
import br.edu.ifba.entities.enums.EventModality;
import br.edu.ifba.entities.enums.EventType;
import br.edu.ifba.entities.enums.PersonType;
import br.edu.ifba.entities.events.Course;
import br.edu.ifba.entities.events.Fair;
import br.edu.ifba.entities.events.Lecture;
import br.edu.ifba.entities.events.Workshop;
import br.edu.ifba.entities.personas.External;
import br.edu.ifba.entities.personas.Student;
import br.edu.ifba.entities.personas.Teacher;
import br.edu.ifba.repositories.impl.EventRepository;
import br.edu.ifba.repositories.impl.PersonRepository;
import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.form.FormField;
import br.edu.ifba.ui.pages.types.FormPage;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import br.edu.ifba.views.impl.MainView;

import java.io.PrintWriter;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PersonCreationFormView extends FormPage implements View {
    public static final String NAME = "NEW_PERSON_FORM_VIEW";

    public PersonCreationFormView() {
        super("Create New Person");
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
                LocalDateTime.now().minusYears(20).toLocalDate()
        ));

        fields.add(FormField.choice("category", "Select Person's Category",
                PersonType.EXTERNAL, PersonType.class, PersonType::getLabel)
        );

        fields.add(FormField.confirmation("confirmCreation", "Are you sure you want to create this person?", true));

        return fields;
    }

    @Override
    protected void onSubmit(InteractionProvider provider, Map<String, Object> results) {
        PrintWriter writer = provider.getWriter();
        writer.println("\n--- Form Submission Processing ---");

        if (results == null || results.isEmpty() || !Boolean.TRUE.equals(results.get("confirmCreation"))) {
            if (results != null && !results.isEmpty() && !Boolean.TRUE.equals(results.get("confirmCreation"))) {
                writer.println("Person creation cancelled by user.");
            } else {
                writer.println("Form was cancelled, an error occurred, or no data was entered.");
            }
        } else {
            writer.println("Received data:");

            results.forEach((key, value) -> {
                if (!key.equals("confirmCreation")) {
                    writer.println(String.format("  %s: %s (Type: %s)",
                            key,
                            value,
                            value != null ? value.getClass().getSimpleName() : "null"));
                }
            });

            PersonType selectedPersonType = (PersonType) results.get("category");

            try {
                Person person = createPersonInstance(selectedPersonType, results);

                PersonRepository.INSTANCE.save(person.getCpf(), person);
                PersonRepository.INSTANCE.persist();

                writer.println(String.format("\nPerson '%s' with CPF %s created successfully!",
                        person.getName(), GlobalScope.CPF_FORMATTER.apply(person.getCpf())));
            } catch (Exception e) {
                writer.println("Error saving person: " + e.getMessage());

                e.printStackTrace(writer);
            }
        }

        writer.println("\nPress Enter to return to the main menu...");
        provider.readLine("");
        ViewRepository.INSTANCE.getById(MainView.NAME).ifPresent(Application::handleContextSwitch);
    }

    protected Person createPersonInstance(PersonType personType, Map<String, Object> results) {
        try {
            if (personType == null) {
                throw new IllegalArgumentException("Person type cannot be null");
            }

            return switch (personType) {
                case EXTERNAL -> External.builder()
                        .cpf((String) results.get("cpf"))
                        .name((String) results.get("name"))
                        .birthDate((LocalDate) results.get("birthDate"))
                        .type(personType)
                        .build();
                case STUDENT -> Student.builder()
                        .cpf((String) results.get("cpf"))
                        .name((String) results.get("name"))
                        .birthDate((LocalDate) results.get("birthDate"))
                        .type(personType)
                        .build();
                case TEACHER -> Teacher.builder()
                        .cpf((String) results.get("cpf"))
                        .name((String) results.get("name"))
                        .birthDate((LocalDate) results.get("birthDate"))
                        .type(personType)
                        .build();
            };
        } catch (Exception e) {
            throw new RuntimeException("Failed to create person instance for type: " + personType, e);
        }
    }

    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new PersonCreationFormView());
    }
}