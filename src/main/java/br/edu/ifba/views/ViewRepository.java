package br.edu.ifba.views;

import br.edu.ifba.repositories.AbstractIdToEntityRepository;
import br.edu.ifba.views.impl.MainView;
import br.edu.ifba.views.impl.forms.EventCreationFormView;

public class ViewRepository extends AbstractIdToEntityRepository<String, View> {
    public static ViewRepository INSTANCE = new ViewRepository();

    public static void initialize() {
        MainView.initialize();
        EventCreationFormView.initialize();
    }

}
