package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Paths;

import java.util.TreeMap;

/**
 * Utility for sorting an {@link OpenAPI} model in-place before serialization.
 *
 * <ul>
 *   <li>Paths are sorted alphabetically by path string.</li>
 *   <li>All component maps (schemas, parameters, requestBodies, responses, headers,
 *       securitySchemes, examples, links, callbacks) are sorted alphabetically by name.</li>
 *   <li>HTTP method ordering within a path is handled by {@link org.openapitools.codegen.serializer.PathItemSerializer},
 *       which writes operations in classical spec order: GET, PUT, POST, DELETE, OPTIONS, HEAD, PATCH, TRACE.</li>
 * </ul>
 */
public class OpenAPISorter {

    private OpenAPISorter() {
    }

    public static void sort(OpenAPI openAPI) {
        if (openAPI == null) {
            return;
        }
        sortPaths(openAPI);
        sortComponents(openAPI);
    }

    private static void sortPaths(OpenAPI openAPI) {
        if (openAPI.getPaths() == null || openAPI.getPaths().isEmpty()) {
            return;
        }
        Paths sorted = new Paths();
        openAPI.getPaths().entrySet().stream()
                .sorted(java.util.Map.Entry.comparingByKey())
                .forEach(e -> sorted.addPathItem(e.getKey(), e.getValue()));
        if (openAPI.getPaths().getExtensions() != null) {
            openAPI.getPaths().getExtensions().forEach(sorted::addExtension);
        }
        openAPI.setPaths(sorted);
    }

    private static void sortComponents(OpenAPI openAPI) {
        Components c = openAPI.getComponents();
        if (c == null) {
            return;
        }
        if (c.getSchemas() != null) {
            c.setSchemas(new TreeMap<>(c.getSchemas()));
        }
        if (c.getParameters() != null) {
            c.setParameters(new TreeMap<>(c.getParameters()));
        }
        if (c.getRequestBodies() != null) {
            c.setRequestBodies(new TreeMap<>(c.getRequestBodies()));
        }
        if (c.getResponses() != null) {
            c.setResponses(new TreeMap<>(c.getResponses()));
        }
        if (c.getHeaders() != null) {
            c.setHeaders(new TreeMap<>(c.getHeaders()));
        }
        if (c.getSecuritySchemes() != null) {
            c.setSecuritySchemes(new TreeMap<>(c.getSecuritySchemes()));
        }
        if (c.getExamples() != null) {
            c.setExamples(new TreeMap<>(c.getExamples()));
        }
        if (c.getLinks() != null) {
            c.setLinks(new TreeMap<>(c.getLinks()));
        }
        if (c.getCallbacks() != null) {
            c.setCallbacks(new TreeMap<>(c.getCallbacks()));
        }
    }
}
