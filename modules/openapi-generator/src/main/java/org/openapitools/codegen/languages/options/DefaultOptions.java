package org.openapitools.codegen.languages.options;

import java.util.*;

public abstract class DefaultOptions implements Options {
    protected Set<String> reservedWords;
    protected Set<String> languageSpecificPrimitives;

    public DefaultOptions() {
        reservedWords = new HashSet<String>();
    }


    protected void setReservedWordsLowerCase(List<String> words) {
        reservedWords = new HashSet<String>();
        for (String word : words) {
            reservedWords.add(word.toLowerCase());
        }
    }

    @Override
    public Set<String> getReservedWords() {
        return reservedWords;
    }

    @Override
    public Set<String> getLanguageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }
}
