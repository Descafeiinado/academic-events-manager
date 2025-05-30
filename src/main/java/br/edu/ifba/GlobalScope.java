package br.edu.ifba;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

public class GlobalScope {
    public static Gson GSON = new GsonBuilder().setPrettyPrinting().create();
}
