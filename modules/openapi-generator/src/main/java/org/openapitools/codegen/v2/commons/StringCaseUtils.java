package org.openapitools.codegen.v2.commons;

import com.google.common.base.CharMatcher;

import java.util.*;

public final class StringCaseUtils {
    private static final CharMatcher NON_ALPHA_MATCHER = CharMatcher
            .inRange('a', 'z')
            .or(CharMatcher.inRange('A', 'Z'))
            .or(CharMatcher.inRange('0', '9'))
            .negate()
            .precomputed();

    private StringCaseUtils() { }

    public static String camelCase(String string) {
        Objects.requireNonNull(string);
        boolean firstPass = true;
        StringBuilder builder = new StringBuilder();
        for (String word : splitWords(string)) {
            word = word.toLowerCase(Locale.getDefault());
            builder.append(firstPass ? word.charAt(0) : Character.toUpperCase(word.charAt(0)));
            builder.append(word, 1, word.length());
            firstPass = false;
        }
        return builder.toString();
    }

    public static String upperCamelCase(String string) {
        Objects.requireNonNull(string);
        StringBuilder builder = new StringBuilder();
        for (String word : splitWords(string)) {
            word = word.toLowerCase(Locale.getDefault());
            builder.append(Character.toUpperCase(word.charAt(0)));
            builder.append(word, 1, word.length());
        }
        return builder.toString();
    }

    public static String snakeCase(String string) {
        Objects.requireNonNull(string);
        StringBuilder builder = new StringBuilder();
        for (String word : splitWords(string)) {
            word = word.toLowerCase(Locale.getDefault());
            builder.append(word);
            builder.append('_');
        }
        if (builder.length() > 1) {
            builder.setLength(builder.length() - 1);
        }
        return builder.toString();
    }

    public static String upperSnakeCase(String string) {
        Objects.requireNonNull(string);
        StringBuilder builder = new StringBuilder();
        for (String word : splitWords(string)) {
            word = word.toUpperCase(Locale.getDefault());
            builder.append(word);
            builder.append('_');
        }
        if (builder.length() > 1) {
            builder.setLength(builder.length() - 1);
        }
        return builder.toString();
    }

    public static Set<String> splitUniqueWords(String string) {
        return new TreeSet<>(splitWords(string));
    }

    public static List<String> splitWords(String string) {
        Objects.requireNonNull(string);
        String[] words = string.split("(?=[^a-z0-9])");
        List<String> curatedWords = new ArrayList<>();
        StringBuilder acronymBuilder = null;
        for (String word : words) {
            if (word.length() == 1 && Character.isUpperCase(word.charAt(0))) {
                if (acronymBuilder == null) {
                    acronymBuilder = new StringBuilder();
                }
                acronymBuilder.append(word);
                continue;
            }
            if (acronymBuilder != null) {
                curatedWords.add(acronymBuilder.toString());
                acronymBuilder = null;
            }
            String curatedWord = NON_ALPHA_MATCHER.trimFrom(word);
            if (!curatedWord.isEmpty()) {
                curatedWords.add(curatedWord);
            }
        }
        if (acronymBuilder != null) {
            curatedWords.add(acronymBuilder.toString());
        }
        return curatedWords;
    }
}
