package org.openapitools.codegen.api;

/**
 * Provides means for searching for "actual" template location based on relative template file.
 */
public interface TemplatePathLocator {
    /**
     * Get the full path to a relative template file.
     *
     * @param relativeTemplateFile Template file
     * @return String Full template file path
     */
    String getFullTemplatePath(String relativeTemplateFile);
}
