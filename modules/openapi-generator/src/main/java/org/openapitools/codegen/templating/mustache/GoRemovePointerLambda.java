package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import java.io.IOException;
import java.io.Writer;
import java.util.regex.Pattern;

/**
 * Mustache lambda that removes pointer prefixes (*) from Go type declarations.
 * This is useful for converting pointer types to value types in generated Go code.
 */
public class GoRemovePointerLambda implements Mustache.Lambda {

    // Constants for Go type patterns
    private static final String ARRAY_PREFIX = "[]";
    private static final String POINTER_ARRAY_PREFIX = "*[]";
    private static final String ANY_TYPE = "any";
    private static final String POINTER_ANY_TYPE = "*any";
    private static final String MAP_TYPE_PREFIX = "map";
    private static final String POINTER_MAP_TYPE_PREFIX = "*map";

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        final String modelType = fragment.execute();
        if (modelType == null || modelType.trim().isEmpty()) {
            writer.write("");
            return;
        }

        String result = removePointers(modelType.trim());
        writer.write(result);
    }

    /**
     * Removes pointer prefixes from Go type declarations.
     * Handles specific cases for arrays, maps, and any types, then applies
     * a general pattern for remaining pointer cases.
     *
     * @param modelType the Go type string that may contain pointer prefixes
     * @return the type string with pointer prefixes removed
     */
    private String removePointers(String modelType) {
        // Handle specific known patterns first for better control

        return modelType
                .replace(POINTER_ARRAY_PREFIX, ARRAY_PREFIX)
                .replace(POINTER_MAP_TYPE_PREFIX, MAP_TYPE_PREFIX)
                .replace(POINTER_ANY_TYPE, ANY_TYPE);
    }
}
