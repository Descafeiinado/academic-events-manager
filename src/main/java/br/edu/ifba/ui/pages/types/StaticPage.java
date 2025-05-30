package br.edu.ifba.ui.pages.types;

import br.edu.ifba.ui.common.InteractionProvider;
import br.edu.ifba.ui.components.text.TextComponent;
import br.edu.ifba.ui.pages.Page;

import java.util.function.Consumer;

public class StaticPage extends Page {

    private Consumer<InteractionProvider> onGoBack;

    public StaticPage(String title) {
        super(title);
    }

    public void addText(String text) {
        addComponent(new TextComponent(text));
    }

    public void setOnGoBack(Consumer<InteractionProvider> onGoBack) {
        this.onGoBack = onGoBack;
    }

    @Override
    public void display(InteractionProvider provider) {
        provider.clearScreen();
        renderTitle(provider);
        renderComponents(provider);

        if (onGoBack != null) provider.getWriter().println("\nPress Enter to go back...");

        provider.readLine("");

        if (onGoBack != null) {
            onGoBack.accept(provider);
        } else provider.getWriter().println("No action defined for going back, application will exit.");
    }

}
