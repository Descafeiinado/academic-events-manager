package br.edu.ifba.views.impl;

import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.text.TextComponent;
import br.edu.ifba.ui.pages.types.NavigationPage;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import br.edu.ifba.views.impl.forms.EventCreationFormView;
import br.edu.ifba.views.impl.forms.PersonCreationFormView;

public class PeopleManagementView extends NavigationPage implements View {
    public static final String NAME = "PEOPLE_MANAGEMENT_VIEW";

    public PeopleManagementView() {
        super("People Management");

        addComponent(new TextComponent("You can manage people here."));
        addComponent(new TextComponent("Please choose an option below:"));
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public void configureNavigationOptions(InteractionProvider provider) {
        addNavigationOption("Create New Person", PersonCreationFormView.NAME);

        addBackOption(provider, "MAIN");
    }

    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new PeopleManagementView());
    }
}