package org.openapitools.codegen.languages.options;

import java.util.Map;
import java.util.Set;

public interface Options {
    Set<String> getReservedWords();

    Set<String> getLanguageSpecificPrimitives();
}
