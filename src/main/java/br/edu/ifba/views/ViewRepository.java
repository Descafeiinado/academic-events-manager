package br.edu.ifba.views;

import br.edu.ifba.repositories.IdToEntityRepository;
import br.edu.ifba.views.impl.EventManagementView;
import br.edu.ifba.views.impl.MainView;
import br.edu.ifba.views.impl.forms.EventCreationFormView;

public class ViewRepository extends IdToEntityRepository<String, View> {
    public static ViewRepository INSTANCE = new ViewRepository();

    public static void initialize() {
        MainView.initialize();
        EventManagementView.initialize();
        EventCreationFormView.initialize();
    }

}
