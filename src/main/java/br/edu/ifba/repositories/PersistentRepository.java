package br.edu.ifba.repositories;

public interface PersistentRepository {
    default String getFileName() {
        String className = this.getClass().getSimpleName();

        return className.substring(0, className.length() - "Repository".length()).toLowerCase() + ".json";
    }

    void load();
    void persist();
}
