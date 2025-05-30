package br.edu.ifba.entities;

import lombok.Getter;

@Getter
public class SequentialIdentifierProvider {

    private Long currentId = 0L;

    public Long getNextId() {
        return ++currentId;
    }

    public Long rollback() {
        if (currentId > 0) {
            currentId--;
        }

        return currentId;
    }

}
