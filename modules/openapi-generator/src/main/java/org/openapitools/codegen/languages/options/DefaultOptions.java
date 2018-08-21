package org.openapitools.codegen.languages.options;

import com.google.common.base.Strings;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Pattern;

public abstract class DefaultOptions implements Options {
    private static final Logger LOGGER = LoggerFactory.getLogger(DefaultOptions.class);

    protected Set<String> reservedWords;
    protected Set<String> languageSpecificPrimitives;
    protected Map<String, String> specialCharReplacements;
    protected boolean allowUnicodeIdentifiers;
    protected boolean supportsInheritance;
    protected boolean supportsMultipleInheritance = false;
    protected boolean supportsMixins;

    public DefaultOptions() {
        reservedWords = new HashSet<String>();

        // Initialize special characters
        specialCharReplacements = new HashMap<>();
        specialCharReplacements.put("$", "Dollar");
        specialCharReplacements.put("^", "Caret");
        specialCharReplacements.put("|", "Pipe");
        specialCharReplacements.put("=", "Equal");
        specialCharReplacements.put("*", "Star");
        specialCharReplacements.put("-", "Minus");
        specialCharReplacements.put("&", "Ampersand");
        specialCharReplacements.put("%", "Percent");
        specialCharReplacements.put("#", "Hash");
        specialCharReplacements.put("@", "At");
        specialCharReplacements.put("!", "Exclamation");
        specialCharReplacements.put("+", "Plus");
        specialCharReplacements.put(":", "Colon");
        specialCharReplacements.put(">", "Greater_Than");
        specialCharReplacements.put("<", "Less_Than");
        specialCharReplacements.put(".", "Period");
        specialCharReplacements.put("_", "Underscore");
        specialCharReplacements.put("?", "Question_Mark");
        specialCharReplacements.put(",", "Comma");
        specialCharReplacements.put("'", "Quote");
        specialCharReplacements.put("\"", "Double_Quote");
        specialCharReplacements.put("/", "Slash");
        specialCharReplacements.put("\\", "Back_Slash");
        specialCharReplacements.put("(", "Left_Parenthesis");
        specialCharReplacements.put(")", "Right_Parenthesis");
        specialCharReplacements.put("{", "Left_Curly_Bracket");
        specialCharReplacements.put("}", "Right_Curly_Bracket");
        specialCharReplacements.put("[", "Left_Square_Bracket");
        specialCharReplacements.put("]", "Right_Square_Bracket");
        specialCharReplacements.put("~", "Tilde");
        specialCharReplacements.put("`", "Backtick");

        specialCharReplacements.put("<=", "Less_Than_Or_Equal_To");
        specialCharReplacements.put(">=", "Greater_Than_Or_Equal_To");
        specialCharReplacements.put("!=", "Not_Equal");
    }


    protected void setReservedWordsLowerCase(List<String> words) {
        reservedWords = new HashSet<String>();
        for (String word : words) {
            reservedWords.add(word.toLowerCase(Locale.ROOT));
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
    public Map<String, String> getSpecialCharReplacements() {
        return specialCharReplacements;
    }

    @Override
    public boolean getSupportsInheritance() {
        return supportsInheritance;
    }

    @Override
    public boolean getSupportsMultipleInheritance() {
        return supportsMultipleInheritance;
    }

    @Override
    public boolean getSupportsMixins() {
        return supportsMixins;
    }

    @Override
    public boolean getAllowUnicodeIdentifiers() {
        return allowUnicodeIdentifiers;
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

    protected boolean isReservedWord(String word) {
        return word != null && reservedWords.contains(word.toLowerCase(Locale.ROOT));
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    @SuppressWarnings("static-method")
    public String sanitizeName(String name) {
        return sanitizeName(name, "\\W");
    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name            string to be sanitize
     * @param removeCharRegEx a regex containing all char that will be removed
     * @return sanitized string
     */
    public String sanitizeName(String name, String removeCharRegEx) {
        // NOTE: performance wise, we should have written with 2 replaceAll to replace desired
        // character with _ or empty character. Below aims to spell out different cases we've
        // encountered so far and hopefully make it easier for others to add more special
        // cases in the future.

        // better error handling when map/array type is invalid
        if (name == null) {
            LOGGER.error("String to be sanitized is null. Default to ERROR_UNKNOWN");
            return "ERROR_UNKNOWN";
        }

        // if the name is just '$', map it to 'value' for the time being.
        if ("$".equals(name)) {
            return "value";
        }

        // input[] => input
        name = name.replaceAll("\\[\\]", ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // input[a][b] => input_a_b
        name = name.replaceAll("\\[", "_");
        name = name.replaceAll("\\]", "");

        // input(a)(b) => input_a_b
        name = name.replaceAll("\\(", "_");
        name = name.replaceAll("\\)", "");

        // input.name => input_name
        name = name.replaceAll("\\.", "_");

        // input-name => input_name
        name = name.replaceAll("-", "_");

        // a|b => a_b
        name = name.replace("|", "_");

        // input name and age => input_name_and_age
        name = name.replaceAll(" ", "_");

        // remove everything else other than word, number and _
        // $php_variable => php_variable
        if (allowUnicodeIdentifiers) { //could be converted to a single line with ?: operator
            name = Pattern.compile(removeCharRegEx, Pattern.UNICODE_CHARACTER_CLASS).matcher(name).replaceAll("");
        } else {
            name = name.replaceAll(removeCharRegEx, "");
        }

        return name;
    }

    /**
     * Return the symbol name of a symbol
     *
     * @param input Symbol (e.g. $)
     * @return Symbol name (e.g. Dollar)
     */
    protected String getSymbolName(String input) {
        return specialCharReplacements.get(input);
    }
}
