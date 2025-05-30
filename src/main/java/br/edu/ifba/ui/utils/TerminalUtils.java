package br.edu.ifba.ui.utils;

public class TerminalUtils {
    public static void clearTerminal() {
        try {
            if (System.getProperty("os.name").toLowerCase().contains("win")) {
                new ProcessBuilder("cmd", "/c", "cls").inheritIO().start().waitFor();
            } else {
                new ProcessBuilder("clear").inheritIO().start().waitFor();
            }
        } catch (Exception e) {
            System.err.println("Error clearing terminal: " + e.getMessage());
        }
    }
}
