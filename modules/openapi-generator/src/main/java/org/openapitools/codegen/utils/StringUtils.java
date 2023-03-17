package org.openapitools.codegen.utils;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Ticker;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.config.GlobalSettings;

import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.CamelizeOption.LOWERCASE_FIRST_LETTER;
import static org.openapitools.codegen.utils.CamelizeOption.UPPERCASE_FIRST_CHAR;

public class StringUtils {

    /**
     * Set the cache size (entry count) of the sanitizedNameCache, camelizedWordsCache and underscoreWordsCache.
     */
    public static final String NAME_CACHE_SIZE_PROPERTY = "org.openapitools.codegen.utils.namecache.cachesize";
    /**
     * Set the cache expiry (in seconds) of the sanitizedNameCache, camelizedWordsCache and underscoreWordsCache.
     */
    public static final String NAME_CACHE_EXPIRY_PROPERTY = "org.openapitools.codegen.utils.namecache.expireafter.seconds";

    // A cache of underscored words, used to optimize the performance of the underscore() method.
    private static Cache<String, String> underscoreWordsCache;

    // A cache of escaped words, used to optimize the performance of the escape() method.
    private static Cache<EscapedNameOptions, String> escapedWordsCache;

    static {
        int cacheSize = Integer.parseInt(GlobalSettings.getProperty(NAME_CACHE_SIZE_PROPERTY, "200"));
        int cacheExpiry = Integer.parseInt(GlobalSettings.getProperty(NAME_CACHE_EXPIRY_PROPERTY, "5"));

        escapedWordsCache = Caffeine.newBuilder()
                .maximumSize(cacheSize)
                .expireAfterAccess(cacheExpiry, TimeUnit.SECONDS)
                .ticker(Ticker.systemTicker())
                .build();

        underscoreWordsCache = Caffeine.newBuilder()
                .maximumSize(cacheSize)
                .expireAfterAccess(cacheExpiry, TimeUnit.SECONDS)
                .ticker(Ticker.systemTicker())
                .build();
    }

    private static Pattern capitalLetterPattern = Pattern.compile("([A-Z]+)([A-Z][a-z][a-z]+)");
    private static Pattern lowercasePattern = Pattern.compile("([a-z\\d])([A-Z])");
    private static Pattern pkgSeparatorPattern = Pattern.compile("\\.");
    private static Pattern dollarPattern = Pattern.compile("\\$");

    /**
     * Underscore the given word.
     * Copied from Twitter elephant bird
     * https://github.com/twitter/elephant-bird/blob/master/core/src/main/java/com/twitter/elephantbird/util/Strings.java
     *
     * @param word The word
     * @return The underscored version of the word
     */
    public static String underscore(final String word) {
        return underscoreWordsCache.get(word, wordToUnderscore -> {
            String result;
            String replacementPattern = "$1_$2";
            // Replace package separator with slash.
            result = pkgSeparatorPattern.matcher(wordToUnderscore).replaceAll("/");
            // Replace $ with two underscores for inner classes.
            result = dollarPattern.matcher(result).replaceAll("__");
            // Replace capital letter with _ plus lowercase letter.
            result = capitalLetterPattern.matcher(result).replaceAll(replacementPattern);
            result = lowercasePattern.matcher(result).replaceAll(replacementPattern);
            result = result.replace('-', '_');
            // replace space with underscore
            result = result.replace(' ', '_');
            result = result.toLowerCase(Locale.ROOT);
            return result;
        });
    }

    /**
     * Dashize the given word.
     *
     * @param word The word
     * @return The dashized version of the word, e.g. "my-name"
     */
    public static String dashize(String word) {
        return underscore(word).replaceAll("[_ ]+", "-");
    }

    /**
     * Camelize name (parameter, property, method, etc) with upper case for first letter
     * copied from Twitter elephant bird
     * https://github.com/twitter/elephant-bird/blob/master/core/src/main/java/com/twitter/elephantbird/util/Strings.java
     *
     * @param word string to be camelize
     * @return camelized string
     */
    public static String camelize(String word) {
        return camelize(word, UPPERCASE_FIRST_CHAR);
    }

    /**
     * Camelize name (parameter, property, method, etc)
     *
     * @param inputWord string to be camelize
     * @param camelizeOption option for the camelize result
     * @return camelized string
     */
    public static String camelize(final String inputWord, CamelizeOption camelizeOption) {
        StringBuilder result = new StringBuilder(inputWord.length());
        boolean capitalizeNext = camelizeOption == UPPERCASE_FIRST_CHAR;
        boolean lowercaseNextLetter = camelizeOption == LOWERCASE_FIRST_LETTER;
        for (int i = 0; i < inputWord.length(); i++) {
            char currentChar = inputWord.charAt(i);
            if (currentChar == '_' || currentChar == '-' || currentChar == '.' || currentChar == '/') {
                if (i == 0) {
                    capitalizeNext = !lowercaseNextLetter;
                } else {
                    capitalizeNext = true;
                }
            } else if ((currentChar == '$' || currentChar == '\\') && i == 0) {
                result.append(currentChar);
                capitalizeNext = !lowercaseNextLetter;
            } else if (capitalizeNext) {
                result.append(Character.toUpperCase(currentChar));
                capitalizeNext = false;
            } else if (Character.isAlphabetic(currentChar) && (i == 0 || lowercaseNextLetter)) {
                result.append(Character.toLowerCase(currentChar));
                lowercaseNextLetter = false;
            } else {
                result.append(currentChar);
            }
        }
        return result.toString();
    }

    private static class EscapedNameOptions {
        public EscapedNameOptions(String name, Set<String> specialChars, List<String> charactersToAllow, String appendToReplacement) {
            this.name = name;
            this.appendToReplacement = appendToReplacement;
            if (specialChars != null) {
                this.specialChars = Collections.unmodifiableSet(specialChars);
            } else {
                this.specialChars = Collections.emptySet();
            }
            if (charactersToAllow != null) {
                this.charactersToAllow = Collections.unmodifiableList(charactersToAllow);
            } else {
                this.charactersToAllow = Collections.emptyList();
            }
        }

        private String name;
        private String appendToReplacement;
        private Set<String> specialChars;
        private List<String> charactersToAllow;

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            EscapedNameOptions that = (EscapedNameOptions) o;
            return Objects.equals(name, that.name) &&
                    Objects.equals(appendToReplacement, that.appendToReplacement) &&
                    Objects.equals(specialChars, that.specialChars) &&
                    Objects.equals(charactersToAllow, that.charactersToAllow);
        }

        @Override
        public int hashCode() {
            return Objects.hash(name, appendToReplacement, specialChars, charactersToAllow);
        }
    }

    /**
     * Return the name with escaped characters.
     *
     * @param name the name to be escaped
     * @param replacementMap map of replacement characters for non-allowed characters
     * @param charactersToAllow characters that are not escaped
     * @param appendToReplacement String to append to replaced characters.
     * @return the escaped word
     * <p>
     * throws Runtime exception as word is not escaped properly.
     */
    public static String escape(final String name, final Map<String, String> replacementMap,
                                final List<String> charactersToAllow, final String appendToReplacement) {
        EscapedNameOptions ns = new EscapedNameOptions(name, replacementMap.keySet(), charactersToAllow, appendToReplacement);
        return escapedWordsCache.get(ns, wordToEscape -> {
            String result = name.chars().mapToObj(c -> {
                String character = String.valueOf((char) c);
                if (charactersToAllow != null && charactersToAllow.contains(character)) {
                    return character;
                } else if (replacementMap.containsKey(character)) {
                    return replacementMap.get(character) + (appendToReplacement != null ? appendToReplacement: "");
                } else {
                    return character;
                }
            }).reduce( (c1, c2) -> c1 + c2).orElse(null);

            if (result != null) return result;
            throw new RuntimeException("Word '" + name + "' could not be escaped.");
        });
    }
}
