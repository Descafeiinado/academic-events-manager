package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.application.GlobalScope;
import br.edu.ifba.aem.infrastructure.services.EventService;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.TextComponent;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;
import br.edu.ifba.aem.ui.views.events.EventDetailsView;
import br.edu.ifba.aem.ui.views.events.EventListView;
import br.edu.ifba.aem.ui.views.forms.EventCreationFormView;

public class EventManagementView extends NavigationPage implements View {

  public static final String NAME = "EVENT_MANAGEMENT_VIEW";

  public static final EventService eventService = EventService.INSTANCE;

  public EventManagementView() {
    super("Event Management");

    addComponent(new TextComponent("You can manage events here."));
    addComponent(new TextComponent("Please choose an option below:"));
  }

  public static void initialize() {
    ViewRepository.INSTANCE.save(NAME, new EventManagementView());
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    addNavigationOption("Create New Event", EventCreationFormView.NAME);
    addCustomAction("View All Events",
        () -> Application.handleContextSwitch(provideEventListView()));

    addBackOption(provider, "MAIN");
  }

  public EventListView provideEventListView() {
    return new EventListView("List of Events", eventService.findAllEvents(),
        event -> String.format("%s | %s: %s", event.getDate().format(GlobalScope.DATE_FORMAT),
            event.getType().getLabel(), event.getTitle()),
        event -> () -> Application.handleContextSwitch(
            new EventDetailsView(event, this)));
  }

}