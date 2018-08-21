package org.openapitools.codegen.languages.options;

import com.google.common.base.Strings;

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

    @Override
    public String sanitizePackageName(String packageName) {
        packageName = packageName.trim(); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        packageName = packageName.replaceAll("[^a-zA-Z0-9_\\.]", "_");
        if (Strings.isNullOrEmpty(packageName)) {
            return "invalidPackageName";
        }
        return packageName;
    }
}
