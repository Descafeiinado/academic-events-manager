package br.edu.ifba.views.impl;

import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.text.TextComponent;
import br.edu.ifba.ui.pages.types.NavigationPage;
import br.edu.ifba.ui.pages.types.StaticPage;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import br.edu.ifba.views.impl.forms.EventCreationFormView;

public class EventManagementView extends NavigationPage implements View {
    public static final String NAME = "EVENT_MANAGEMENT_VIEW";

    public EventManagementView() {
        super("Event Management");

        addComponent(new TextComponent("You can manage events here."));
        addComponent(new TextComponent("Please choose an option below:"));
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

    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new EventManagementView());
    }
}