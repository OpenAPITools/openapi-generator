package org.openapitools.codegen.utils;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringUtils {
    // A cache of camelized words. The camelize() method is invoked many times with the same
    // arguments, this cache is used to optimized performance.
    private static Map<Boolean, Map<String, String>> camelizedWords =
        new HashMap<Boolean, Map<String, String>>();

    // A cache of underscored words, used to optimize the performance of the underscore() method.
    private static Map<String, String> underscoreWords = new HashMap<String, String>();

    static {
        camelizedWords.put(false, new HashMap<String, String>());
        camelizedWords.put(true, new HashMap<String, String>());
    }

    /**
     * Underscore the given word.
     * Copied from Twitter elephant bird
     * https://github.com/twitter/elephant-bird/blob/master/core/src/main/java/com/twitter/elephantbird/util/Strings.java
     *
     * @param word The word
     * @return The underscored version of the word
     */
    public static String underscore(final String word) {
        String result = underscoreWords.get(word);
        if (result != null) {
            return result;
        }
        String firstPattern = "([A-Z]+)([A-Z][a-z])";
        String secondPattern = "([a-z\\d])([A-Z])";
        String replacementPattern = "$1_$2";
        // Replace package separator with slash.
        result = word.replaceAll("\\.", "/");
        // Replace $ with two underscores for inner classes.
        result = result.replaceAll("\\$", "__");
        // Replace capital letter with _ plus lowercase letter.
        result = result.replaceAll(firstPattern, replacementPattern);
        result = result.replaceAll(secondPattern, replacementPattern);
        result = result.replace('-', '_');
        // replace space with underscore
        result = result.replace(' ', '_');
        result = result.toLowerCase(Locale.ROOT);
        underscoreWords.put(word, result);
        return result;
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
        return camelize(word, false);
    }

    private static Pattern camelizeSlashPattern = Pattern.compile("\\/(.?)");
    private static Pattern camelizeUppercasePattern = Pattern.compile("(\\.?)(\\w)([^\\.]*)$");
    private static Pattern camelizeUnderscorePattern = Pattern.compile("(_)(.)");
    private static Pattern camelizeHyphenPattern = Pattern.compile("(-)(.)");

    /**
     * Camelize name (parameter, property, method, etc)
     *
     * @param word                 string to be camelize
     * @param lowercaseFirstLetter lower case for first letter if set to true
     * @return camelized string
     */
    public static String camelize(String word, boolean lowercaseFirstLetter) {
        String inputWord = word;
        String camelized = camelizedWords.get(lowercaseFirstLetter).get(word);
        if (camelized != null) {
            return camelized;
        }
        // Replace all slashes with dots (package separator)
        Matcher m = camelizeSlashPattern.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("." + m.group(1)/*.toUpperCase()*/); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
            m = camelizeSlashPattern.matcher(word);
        }

        // case out dots
        String[] parts = word.split("\\.");
        StringBuilder f = new StringBuilder();
        for (String z : parts) {
            if (z.length() > 0) {
                f.append(Character.toUpperCase(z.charAt(0))).append(z.substring(1));
            }
        }
        word = f.toString();

        m = camelizeSlashPattern.matcher(word);
        while (m.find()) {
            word = m.replaceFirst("" + Character.toUpperCase(m.group(1).charAt(0)) + m.group(1).substring(1)/*.toUpperCase()*/);
            m = camelizeSlashPattern.matcher(word);
        }

        // Uppercase the class name.
        m = camelizeUppercasePattern.matcher(word);
        if (m.find()) {
            String rep = m.group(1) + m.group(2).toUpperCase(Locale.ROOT) + m.group(3);
            rep = rep.replaceAll("\\$", "\\\\\\$");
            word = m.replaceAll(rep);
        }

        // Remove all underscores (underscore_case to camelCase)
        m = camelizeUnderscorePattern.matcher(word);
        while (m.find()) {
            String original = m.group(2);
            String upperCase = original.toUpperCase(Locale.ROOT);
            if (original.equals(upperCase)) {
                word = word.replaceFirst("_", "");
            } else {
                word = m.replaceFirst(upperCase);
            }
            m = camelizeUnderscorePattern.matcher(word);
        }

        // Remove all hyphens (hyphen-case to camelCase)
        m = camelizeHyphenPattern.matcher(word);
        while (m.find()) {
            word = m.replaceFirst(m.group(2).toUpperCase(Locale.ROOT));
            m = camelizeHyphenPattern.matcher(word);
        }

        if (lowercaseFirstLetter && word.length() > 0) {
            int i = 0;
            char charAt = word.charAt(i);
            while (i + 1 < word.length() && !((charAt >= 'a' && charAt <= 'z') || (charAt >= 'A' && charAt <= 'Z'))) {
                i = i + 1;
                charAt = word.charAt(i);
            }
            i = i + 1;
            word = word.substring(0, i).toLowerCase(Locale.ROOT) + word.substring(i);
        }

        // remove all underscore
        word = word.replaceAll("_", "");

        // Add to the cache.
        camelizedWords.get(lowercaseFirstLetter).put(inputWord, word);
        return word;
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
    public static String escape(String name, Map<String, String> replacementMap,
                                List<String> charactersToAllow, String appendToReplacement) {
        String result = name.chars().mapToObj(c -> {
            String character = "" + (char) c;
            if (charactersToAllow != null && charactersToAllow.contains(character)) {
                return character;
            } else if (replacementMap.containsKey(character)) {
                return replacementMap.get(character) + (appendToReplacement != null ? appendToReplacement: "");
            } else {
                return character;
            }
        }).reduce( (c1, c2) -> "" + c1 + c2).orElse(null);

        if (result != null) return result;
        throw new RuntimeException("Word '" + name + "' could not be escaped.");
    }
}
