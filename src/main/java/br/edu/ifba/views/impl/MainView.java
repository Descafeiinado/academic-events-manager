package br.edu.ifba.views.impl;

import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.text.TextComponent;
import br.edu.ifba.ui.pages.types.NavigationPage;
import br.edu.ifba.ui.pages.types.StaticPage;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;

public class MainView extends NavigationPage implements View {
    public static final String NAME = "MAIN";

    public MainView() {
        super("Main Menu");

        addComponent(new TextComponent("Welcome to the Event Management System!"));
        addComponent(new TextComponent("Please choose an option below:"));
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public void configureNavigationOptions(InteractionProvider provider) {
        addNavigationOption("Manage Events", EventManagementView.NAME);
        addNavigationOption("Manage People", PeopleManagementView.NAME);
        addNavigationOption("Generate Reports", "STATIC_DEMO_VIEW");

        addExitOption(provider);
    }

    public static void initialize() {
        ViewRepository.INSTANCE.save(NAME, new MainView());

        // BOILERPLATE: Create a static demo page

        StaticPage demoPage = new StaticPage("Static Demo Page");

        demoPage.addText("This is a simple static page.");
        demoPage.addText("It demonstrates how to display read-only information.");

        View staticDemoView = new View() {
            private static final String DEMO_VIEW_NAME = "STATIC_DEMO_VIEW";

            @Override
            public String getName() {
                return DEMO_VIEW_NAME;
            }

            @Override
            public void display(InteractionProvider provider) {
                demoPage.display(provider);
            }
        };

        ViewRepository.INSTANCE.save(staticDemoView.getName(), staticDemoView);
    }
}