package org.openapitools.codegen.api;

import java.nio.file.Path;

/**
 * interface to the full template content
 * implementers might take into account the -t cli option,
 * look in the resources for a generator specific template, etc
 */
public interface TemplatingExecutor {
    /**
     * returns the template content by name
     *
     * @param name the template name (e.g. model.mustache)
     * @return the contents of that template
     */
    String getFullTemplateContents(String name);

    /**
     * Returns the path of a template, allowing access to the template where consuming literal contents aren't desirable or possible.
     *
     * @param name the template name (e.g. model.mustache)
     * @return The {@link Path} to the template
     */
    Path getFullTemplatePath(String name);
}