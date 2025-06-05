package br.edu.ifba.aem.ui.components;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import java.io.PrintWriter;

public class TextComponent extends Component {

  private String text;

  public TextComponent(String text) {
    this(null, text); // ID can be null if not explicitly needed
  }

  public TextComponent(String id, String text) {
    super(id);
    this.text = text;
  }

  public String getText() {
    return text;
  }

  public void setText(String text) {
    this.text = text;
  }

  @Override
  public void render(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println(text);
  }
}