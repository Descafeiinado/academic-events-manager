package br.edu.ifba.aem.ui.views;

import br.edu.ifba.aem.ui.common.InteractionProvider;

public interface View {

  String getName();

  void display(InteractionProvider provider);
}