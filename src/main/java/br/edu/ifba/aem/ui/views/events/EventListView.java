package br.edu.ifba.aem.ui.views.events;

import br.edu.ifba.aem.application.Application;
import br.edu.ifba.aem.domain.entities.Event;
import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.pages.types.NavigationPage;
import br.edu.ifba.aem.ui.views.EventManagementView;
import br.edu.ifba.aem.ui.views.View;
import br.edu.ifba.aem.ui.views.ViewRepository;
import java.util.List;
import java.util.function.Function;

public class EventListView extends NavigationPage implements View {

  public static final String NAME = "LIST_EVENTS_VIEW";

  private final List<Event> events;
  private final Function<Event, String> eventNameProvider;
  private final Function<Event, Runnable> eventActionProvider;

  private View returnView;

  public EventListView(String title, List<Event> events, Function<Event, String> eventNameProvider,
      View returnView) {
    this(title, events, eventNameProvider,
        event -> () -> Application.handleContextSwitch(new EventDetailsView(event, returnView)),
        returnView);

    this.returnView = returnView;
  }

  public EventListView(String title, List<Event> events, Function<Event, String> eventNameProvider,
      Function<Event, Runnable> eventActionProvider) {
    this(title, events, eventNameProvider, eventActionProvider,
        ViewRepository.INSTANCE.getById(EventManagementView.NAME).orElseThrow(
            () -> new IllegalStateException("EventManagementView not found in ViewRepository.")));
  }

  public EventListView(String title, List<Event> events, Function<Event, String> eventNameProvider,
      Function<Event, Runnable> eventActionProvider, View returnView) {
    super(title);

    this.events = events;
    this.eventNameProvider = eventNameProvider;
    this.eventActionProvider = eventActionProvider;
    this.returnView = returnView;
  }

  public static void initialize() {
  }

  @Override
  public String getName() {
    return NAME;
  }

  @Override
  public void configureNavigationOptions(InteractionProvider provider) {
    for (Event event : events) {
      addCustomAction(eventNameProvider.apply(event), eventActionProvider.apply(event));
    }

    addBackOption(provider, returnView.getName());
  }

}