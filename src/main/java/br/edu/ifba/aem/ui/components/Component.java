package br.edu.ifba.aem.ui.components;

import br.edu.ifba.aem.ui.common.InteractionProvider;

public abstract class Component {

  protected String id;

  public Component(String id) {
    this.id = id;
  }

  public String getId() {
    return id;
  }

  public abstract void render(InteractionProvider provider);
}