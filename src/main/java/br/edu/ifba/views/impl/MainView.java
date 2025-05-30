package br.edu.ifba.views.impl;

import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.text.TextComponent;
import br.edu.ifba.ui.pages.types.NavigationPage;
import br.edu.ifba.ui.pages.types.StaticPage;
import br.edu.ifba.views.View;
import br.edu.ifba.views.ViewRepository;
import br.edu.ifba.views.impl.forms.EventCreationFormView;

public class MainView extends NavigationPage implements View {
    public static final String NAME = "MAIN";

    public MainView() {
        super("Main Menu"); // Page title
        // Add static text components that appear above the navigation options
        addComponent(new TextComponent("Welcome to the Event Management System!"));
        addComponent(new TextComponent("Please choose an option below:"));
    }

    @Override
    public String getName() {
        return NAME;
    }

    @Override
    public void configureNavigationOptions(InteractionProvider provider) {
        // Define navigation options for the main menu
        addNavigationOption("Create New Event", EventCreationFormView.NAME); // Example navigation
        addNavigationOption("View Events (Static Demo)", "STATIC_DEMO_VIEW"); // Placeholder for another view
        // addNavigationOption("Manage Participants", "PARTICIPANT_VIEW_NAME_HERE");
        // addNavigationOption("Generate Reports", "REPORT_VIEW_NAME_HERE");

        addExitOption(provider); // Adds a standardized exit option
    }

    // The 'display' method is inherited from NavigationPage and will use the configured options.

    public static void initialize() {
        // Register this view with the repository
        ViewRepository.INSTANCE.save(NAME, new MainView());

        // Example of initializing a StaticPage view for the demo link
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
                demoPage.display(provider); // Delegate to the StaticPage's display logic
            }
        };

        ViewRepository.INSTANCE.save(staticDemoView.getName(), staticDemoView);    }
}