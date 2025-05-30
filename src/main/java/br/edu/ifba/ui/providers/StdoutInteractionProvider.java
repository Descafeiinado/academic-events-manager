package br.edu.ifba.ui.providers;

import br.edu.ifba.ui.common.InteractionProvider;

import java.io.PrintWriter;
import java.util.Scanner;

public class StdoutInteractionProvider implements InteractionProvider {
    private final Scanner scanner;
    private final PrintWriter writer;

    public StdoutInteractionProvider() {
        this.scanner = new Scanner(System.in);
        this.writer = new PrintWriter(System.out, true);
    }

    @Override
    public PrintWriter getWriter() {
        return writer;
    }

    @Override
    public String readLine(String prompt) {
        writer.print(prompt);

        return scanner.nextLine();
    }

    @Override
    public void clearScreen() {
        writer.print("\033[H\033[2J");
        writer.flush();
    }

    @Override
    public Object getNativeProvider() {
        return this;
    }

    public Scanner getScanner() {
        return scanner;
    }
}