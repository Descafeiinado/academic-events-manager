package br.edu.ifba.aem.ui.pages;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import br.edu.ifba.aem.ui.components.Component;
import java.util.ArrayList;
import java.util.List;
import org.jline.utils.AttributedString;
import org.jline.utils.AttributedStyle;

public abstract class Page {

  protected String title;
  protected List<Component> components;

  public Page(String title) {
    this.title = title;
    this.components = new ArrayList<>();
  }

  public void addComponent(Component component) {
    this.components.add(component);
  }

  public abstract void display(InteractionProvider provider);

  protected void renderTitle(InteractionProvider provider) {
    if (title != null && !title.isEmpty()) {
      provider.getWriter().println("\n=== " + title.toUpperCase() + " ===");
      provider.getWriter().println();
    }
  }

  protected AttributedString build(String text, AttributedStyle style) {
    return new AttributedString(text, style);
  }

  protected AttributedString buildTitle(String title) {
    return new AttributedString("=== " + title + " ===\n\n",
        AttributedStyle.BOLD.foreground(AttributedStyle.YELLOW));
  }

  protected AttributedString buildTitle() {
    return buildTitle(title);
  }

  protected void renderComponents(InteractionProvider provider) {
    for (Component component : components) {
      component.render(provider);
    }

    if (!components.isEmpty()) {
      provider.getWriter().println();
    }
  }
}