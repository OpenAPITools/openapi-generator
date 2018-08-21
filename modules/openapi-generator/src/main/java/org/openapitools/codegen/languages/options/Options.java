package org.openapitools.codegen.languages.options;

import java.util.Map;
import java.util.Set;

public interface Options {
    Set<String> getReservedWords();

    Set<String> getLanguageSpecificPrimitives();

    String sanitizePackageName(String packageName);

    String escapeReservedWord(String name);

    Map<String,String> getSpecialCharReplacements();

    String toVarName(String name);

    String sanitizeTag(String tag);

    String toEnumValue(String value, String datatype);

    String escapeText(String input);

    String escapeUnsafeCharacters(String input);
}
