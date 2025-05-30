package br.edu.ifba.views;

import br.edu.ifba.ui.common.InteractionProvider;

public interface View {
    String getName(); // Unique identifier for the view
    void display(InteractionProvider provider); // Main method to render and handle the view
}