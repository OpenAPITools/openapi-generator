package org.openapitools.codegen.languages.rules;

import java.util.Map;
import java.util.Set;

public interface LanguageRules {
    Set<String> getReservedWords();

    Set<String> getLanguageSpecificPrimitives();

    boolean getAllowUnicodeIdentifiers();

    boolean getSupportsInheritance();

    boolean getSupportsMixins();

    boolean getSupportsMultipleInheritance();
    
    String sanitizePackageName(String packageName);

    String escapeReservedWord(String name);

    Map<String,String> getSpecialCharReplacements();

    String toVarName(String name);

    String sanitizeTag(String tag);

    String toEnumValue(String value, String datatype);

    String escapeText(String input);

    String escapeUnsafeCharacters(String input);

    String toEnumVarName(String value, String datatype);

    String toParamName(String name);

    String toEnumName(String name);
}
