package br.edu.ifba.aem.ui.components;

import br.edu.ifba.aem.ui.common.InteractionProvider;
import lombok.Getter;
import lombok.Setter;
import java.io.PrintWriter;

@Getter
@Setter
public class TextComponent extends Component {

  private String text;

  public TextComponent(String text) {
    this(null, text);
  }

  public TextComponent(String id, String text) {
    super(id);
    this.text = text;
  }

  @Override
  public void render(InteractionProvider provider) {
    PrintWriter writer = provider.getWriter();

    writer.println(text);
  }
}