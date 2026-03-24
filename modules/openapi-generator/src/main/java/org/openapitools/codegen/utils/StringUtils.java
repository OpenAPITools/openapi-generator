package org.openapitools.codegen.utils;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.Ticker;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.openapitools.codegen.config.GlobalSettings;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    // A cache of camelized words. The camelize() method is invoked many times with the same
    // arguments, this cache is used to optimized performance.
    private static Cache<Pair<String, CamelizeOption>, String> camelizedWordsCache;

    // A cache of underscored words, used to optimize the performance of the underscore() method.
    private static Cache<String, String> underscoreWordsCache;

    // A cache of escaped words, used to optimize the performance of the escape() method.
    private static Cache<EscapedNameOptions, String> escapedWordsCache;

    static {
        int cacheSize = Integer.parseInt(GlobalSettings.getProperty(NAME_CACHE_SIZE_PROPERTY, "200"));
        int cacheExpiry = Integer.parseInt(GlobalSettings.getProperty(NAME_CACHE_EXPIRY_PROPERTY, "5"));
        camelizedWordsCache = Caffeine.newBuilder()
                .maximumSize(cacheSize)
                .expireAfterAccess(cacheExpiry, TimeUnit.SECONDS)
                .ticker(Ticker.systemTicker())
                .build();

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

    private static final Pattern CAPITAL_LETTER_PATTERN = Pattern.compile("([A-Z]+)([A-Z][a-z][a-z]+)");
    private static final Pattern LOWERCASE_PATTERN = Pattern.compile("([a-z\\d])([A-Z])");
    private static final Pattern PKG_SEPARATOR_PATTERN = Pattern.compile("\\.");
    private static final Pattern DOLLAR_PATTERN = Pattern.compile("\\$");

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
            result = PKG_SEPARATOR_PATTERN.matcher(wordToUnderscore).replaceAll("/");
            // Replace $ with two underscores for inner classes.
            result = DOLLAR_PATTERN.matcher(result).replaceAll("__");
            // Replace capital letter with _ plus lowercase letter.
            result = CAPITAL_LETTER_PATTERN.matcher(result).replaceAll(replacementPattern);
            result = LOWERCASE_PATTERN.matcher(result).replaceAll(replacementPattern);
            result = result.replace('-', '_');
            // replace space with underscore
            result = result.replace(' ', '_');
            result = result.toLowerCase(Locale.ROOT);
            return result;
        });
    }

    private static final Pattern UNDERSCORE_OR_SPACE = Pattern.compile("[_ ]+");
    /**
     * Dashize the given word.
     *
     * @param word The word
     * @return The dashized version of the word, e.g. "my-name"
     */
    public static String dashize(String word) {
        return UNDERSCORE_OR_SPACE.matcher(underscore(word)).replaceAll("-");
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

    private static final Pattern CAMELIZE_SLASH_PATTERN = Pattern.compile("/(.?)");
    private static final Pattern CAMELIZE_UPPERCASE_PATTERN = Pattern.compile("(\\.?)(\\w)([^.]*)$");
    private static final Pattern CAMELIZE_UNDERSCORE_PATTERN = Pattern.compile("(_)(.)");
    private static final Pattern CAMELIZE_HYPHEN_PATTERN = Pattern.compile("(-)(.)");
    private static final Pattern CAMELIZE_DOLLAR_PATTERN = Pattern.compile("\\$");
    private static final Pattern CAMELIZE_SIMPLE_UNDERSCORE_PATTERN = Pattern.compile("_");
    private static final Pattern DOT = Pattern.compile("\\.");

    /**
     * Camelize name (parameter, property, method, etc)
     *
     * @param inputWord      string to be camelize
     * @param camelizeOption option for the camelize result
     * @return camelized string
     */
    public static String camelize(final String inputWord, CamelizeOption camelizeOption) {
        Pair<String, CamelizeOption> key = new ImmutablePair<>(inputWord, camelizeOption);

        return camelizedWordsCache.get(key, pair -> {
            String word = pair.getKey();
            CamelizeOption option = pair.getValue();
            // Replace all slashes with dots (package separator)
            Matcher m = CAMELIZE_SLASH_PATTERN.matcher(word);
            while (m.find()) {
                word = m.replaceFirst("." + m.group(1).replace("\\", "\\\\")/*.toUpperCase()*/);
                m = CAMELIZE_SLASH_PATTERN.matcher(word);
            }

            // case out dots
            String[] parts = DOT.split(word);
            StringBuilder f = new StringBuilder();
            for (String z : parts) {
                if (z.length() > 0) {
                    f.append(Character.toUpperCase(z.charAt(0))).append(z.substring(1));
                }
            }
            word = f.toString();

            m = CAMELIZE_SLASH_PATTERN.matcher(word);
            while (m.find()) {
                word = m.replaceFirst(Character.toUpperCase(m.group(1).charAt(0)) + m.group(1).substring(1)/*.toUpperCase()*/);
                m = CAMELIZE_SLASH_PATTERN.matcher(word);
            }

            // Uppercase the class name.
            m = CAMELIZE_UPPERCASE_PATTERN.matcher(word);
            if (m.find()) {
                String rep = m.group(1) + m.group(2).toUpperCase(Locale.ROOT) + m.group(3);
                rep = CAMELIZE_DOLLAR_PATTERN.matcher(rep).replaceAll("\\\\\\$");
                word = m.replaceAll(rep);
            }

            // Remove all underscores (underscore_case to camelCase)
            m = CAMELIZE_UNDERSCORE_PATTERN.matcher(word);
            while (m.find()) {
                String original = m.group(2);
                String upperCase = original.toUpperCase(Locale.ROOT);
                if (original.equals(upperCase)) {
                    word = CAMELIZE_SIMPLE_UNDERSCORE_PATTERN.matcher(word).replaceFirst("");
                } else {
                    word = m.replaceFirst(upperCase);
                }
                m = CAMELIZE_UNDERSCORE_PATTERN.matcher(word);
            }

            // Remove all hyphens (hyphen-case to camelCase)
            m = CAMELIZE_HYPHEN_PATTERN.matcher(word);
            while (m.find()) {
                word = m.replaceFirst(m.group(2).toUpperCase(Locale.ROOT));
                m = CAMELIZE_HYPHEN_PATTERN.matcher(word);
            }

            switch (option) {
                case LOWERCASE_FIRST_LETTER:
                    word = lowercaseFirstLetter(word);
                    break;
                case LOWERCASE_FIRST_CHAR:
                    word = word.substring(0, 1).toLowerCase(Locale.ROOT) + word.substring(1);
                    break;
            }

            // remove all underscore
            word = CAMELIZE_SIMPLE_UNDERSCORE_PATTERN.matcher(word).replaceAll("");
            return word;
        });
    }

    private static String lowercaseFirstLetter(String word) {
        if (word.length() > 0) {
            int i = 0;
            char charAt = word.charAt(i);
            while (i + 1 < word.length() && !((charAt >= 'a' && charAt <= 'z') || (charAt >= 'A' && charAt <= 'Z'))) {
                i = i + 1;
                charAt = word.charAt(i);
            }
            i = i + 1;
            word = word.substring(0, i).toLowerCase(Locale.ROOT) + word.substring(i);
        }
        return word;
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
     * @param name                the name to be escaped
     * @param replacementMap      map of replacement characters for non-allowed characters
     * @param charactersToAllow   characters that are not escaped
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
                    return replacementMap.get(character) + (appendToReplacement != null ? appendToReplacement : "");
                } else {
                    return character;
                }
            }).reduce((c1, c2) -> c1 + c2).orElse(null);

            if (result != null) return result;
            throw new RuntimeException("Word '" + name + "' could not be escaped.");
        });
    }

    /**
     * Return a unique string based on a set of processed strings.
     *
     * @param processedStrings a set of strings that have been processed
     * @param input            input to be checked for uniqueness
     * @return a unique string
     */
    public static String getUniqueString(Set<String> processedStrings, String input) {
        if (input == null) {
            return null;
        }

        String uniqueName = input;
        // check for input uniqueness
        int counter = 0;

        if (processedStrings.contains(uniqueName)) {
            // look for next unique next, e.g. getName_7
            while (processedStrings.contains(uniqueName)) {
                uniqueName = uniqueName + "_" + counter;
                counter++;
            }
        }

        processedStrings.add(uniqueName);
        return uniqueName;
    }
}
