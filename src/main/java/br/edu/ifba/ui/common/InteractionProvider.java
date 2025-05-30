package br.edu.ifba.ui.common;

import java.io.PrintWriter;

public interface InteractionProvider {
    PrintWriter getWriter();

    String readLine(String prompt);
    void clearScreen();

    Object getNativeProvider();
}