package br.edu.ifba.ui.components.text;

import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.Component;

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

    public void setText(String text) {
        this.text = text;
    }

    public String getText() {
        return text;
    }

    @Override
    public void render(InteractionProvider provider) {
        PrintWriter writer = provider.getWriter();

        writer.println(text);
    }
}