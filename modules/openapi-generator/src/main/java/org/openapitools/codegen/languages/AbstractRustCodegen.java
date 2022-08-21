package org.openapitools.codegen.languages;

import com.google.common.base.Strings;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.function.Function;

import static org.openapitools.codegen.utils.StringUtils.*;

public abstract class AbstractRustCodegen extends DefaultCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(AbstractRustCodegen.class);

    protected List<String> charactersToAllow = Collections.singletonList("_");
    protected Set<String> keywordsThatDoNotSupportRawIdentifiers = new HashSet<>(
            Arrays.asList("super", "self", "Self", "extern", "crate"));
    protected String enumSuffix = "";

    public AbstractRustCodegen() {
        super();
        // All 'Strict' and 'Reserved' keywords from https://doc.rust-lang.org/reference/keywords.html
        // Note: These are case-sensitive
        this.reservedWords = new HashSet<>(
                Arrays.asList(
                        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
                        "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
                        "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
                        "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
                        "override", "priv", "typeof", "unsized", "virtual", "yield", "try"
                )
        );
    }

    @Override
    public boolean isReservedWord(String word) {
        // This is overridden to take account of Rust reserved words being case-sensitive.
        return word != null && reservedWords.contains(word);
    }

    public enum CasingType {CAMEL_CASE, SNAKE_CASE};

    /**
     * General purpose sanitizing function for Rust identifiers (fields, variables, structs, parameters, etc.).<br>
     * Rules for Rust are fairly simple:
     * <ul>
     *     <li>Characters must belong to [A-Za-z0-9_]
     *     <li>Cannot use reserved words (but can sometimes prefix with "r#")
     *     <li>Cannot begin with a number
     * </ul>
     * @param name The input string
     * @param casingType Which casing type to apply
     * @param escapePrefix Prefix to escape words beginning with numbers or reserved words
     * @param type The type of identifier (used for logging)
     * @param allowRawIdentifiers Raw identifiers can't always be used, because of filename vs import mismatch.
     * @return Sanitized string
     */
    public String sanitizeIdentifier(String name, CasingType casingType, String escapePrefix, String type, boolean allowRawIdentifiers) {
        String originalName = name;

        Function<String, String> casingFunction;
        switch (casingType) {
            case CAMEL_CASE:
                // This probably seems odd, but it is necessary for two reasons
                // Compatibility with rust-server, such that MyIDList => my_id_list => MyIdList
                // Conversion from SCREAMING_SNAKE_CASE to ScreamingSnakeCase
                casingFunction = (input) -> camelize(underscore(input));
                break;
            case SNAKE_CASE:
                casingFunction = StringUtils::underscore;
                break;
            default:
                throw new IllegalArgumentException("Unknown CasingType");
        }

        // Replace hyphens with underscores
        name = name.replaceAll("-", "_");

        // Apply special character escapes, e.g. "@type" => "At_type"
        // Remove the trailing underscore if necessary
        if (!Strings.isNullOrEmpty(name)) {
            boolean endedWithUnderscore = name.endsWith("_");
            name = escape(name, specialCharReplacements, charactersToAllow, "_");
            if (!endedWithUnderscore && name.endsWith("_")) {
                name = org.apache.commons.lang3.StringUtils.chop(name);
            }
        }

        // Sanitize any other special characters that weren't replaced
        name = sanitizeName(name);

        // Keep track of modifications prior to casing
        boolean nameWasModified = !originalName.equals(name);

        // Convert casing
        name = casingFunction.apply(name);

        // If word starts with number add a prefix
        // Note: this must be done after casing since CamelCase will strip leading underscores
        if (name.matches("^\\d.*")) {
            nameWasModified = true;
            name = casingFunction.apply(escapePrefix + '_' + name);
        }

        // Escape reserved words - this is case-sensitive so must be done after casing
        // There is currently a bug in Rust where this doesn't work for a few reserved words :(
        // https://internals.rust-lang.org/t/raw-identifiers-dont-work-for-all-identifiers/9094
        if (isReservedWord(name)) {
            nameWasModified = true;
            if (this.keywordsThatDoNotSupportRawIdentifiers.contains(name) || !allowRawIdentifiers) {
                name = casingFunction.apply(escapePrefix + '_' + name);
            } else {
                name = "r#" + name;
            };
        }

        // If the name had to be modified (not just because of casing), log the change
        if (nameWasModified) {
            LOGGER.warn("{} cannot be used as a {} name. Renamed to {}", casingFunction.apply(originalName), type, name);
        }

        return name;
    }

    @Override
    public String toVarName(String name) {
        return sanitizeIdentifier(name, CasingType.SNAKE_CASE, "param", "field/variable", true);
    }

    @Override
    public String toParamName(String name) {
        return sanitizeIdentifier(name, CasingType.SNAKE_CASE, "param", "parameter", true);
    }

    @Override
    public String toOperationId(String operationId) {
        return sanitizeIdentifier(operationId, CasingType.SNAKE_CASE, "call", "method", true);
    }

    //// Model naming ////

    protected String addModelNamePrefixAndSuffix(String name) {
        if (!Strings.isNullOrEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }
        if (!Strings.isNullOrEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }
        return name;
    }

    @Override
    public String toModelName(String name) {
        return sanitizeIdentifier(addModelNamePrefixAndSuffix(name), CasingType.CAMEL_CASE, "model", "model", false);
    }

    @Override
    public String toModelFilename(String name) {
        return sanitizeIdentifier(addModelNamePrefixAndSuffix(name), CasingType.SNAKE_CASE, "model", "model file", false);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    //// Enum naming ////

    @Override
    public String toEnumVarName(String name, String datatype) {
        // Rust Enum variants should be camel cased
        return sanitizeIdentifier(name, CasingType.CAMEL_CASE, "variant", "enum variant", true);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        // Note: Strangely this function is only used for inline enums, schema enums go through the toModelName function
        String name = property.name;
        if (!Strings.isNullOrEmpty(enumSuffix)) {
            name = name + "_" + enumSuffix;
        }
        return sanitizeIdentifier(name, CasingType.CAMEL_CASE, "enum", "enum", false);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        // This is the representation of the enum that will be serialized / deserialized
        // Note: generators currently only support string enums, so checking the type here is pointless
        return escapeText(value);
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        // TODO: Bug: currently the templates ignore this function and just use `Self::{{ enumVars.0.name }}`
        // Return the Rust type name of the variant so we can `impl Default` with it
        return toEnumVarName(value, datatype);
    }

    //// API naming ////

    protected String addApiNamePrefixAndSuffix(String name) {
        if (Strings.isNullOrEmpty(name)) {
            name = "default";
        }
        if (!Strings.isNullOrEmpty(apiNamePrefix)) {
            name = apiNamePrefix + "_" + name;
        }
        if (!Strings.isNullOrEmpty(apiNameSuffix)) {
            name = name + "_" + apiNameSuffix;
        }
        return name;
    }

    @Override
    public String toApiName(String name) {
        return sanitizeIdentifier(addApiNamePrefixAndSuffix(name), CasingType.CAMEL_CASE, "api", "API", false);
    }

    @Override
    public String toApiFilename(String name) {
        return sanitizeIdentifier(addApiNamePrefixAndSuffix(name), CasingType.SNAKE_CASE, "api", "API file", false);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

}
