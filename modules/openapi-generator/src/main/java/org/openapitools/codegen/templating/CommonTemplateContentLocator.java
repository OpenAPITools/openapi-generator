package org.openapitools.codegen.templating;

import com.google.common.io.Resources;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.api.TemplatePathLocator;

import java.io.File;
import java.net.URL;

/**
 * Locates generator-agnostic templates from a common built-in location.
 */
public class CommonTemplateContentLocator implements TemplatePathLocator {
    private String resourceLocation = "_common";

    /**
     * Constructs a new instance of {@link CommonTemplateContentLocator} defaulting to _common resource location.
     */
    public CommonTemplateContentLocator() {

    }

    /**
     * Constructs a new instance of {@link CommonTemplateContentLocator} for a targeted common location.
     *
     * @param resourceLocation A custom common file location.
     */
    public CommonTemplateContentLocator(String resourceLocation) {
        this.resourceLocation = resourceLocation;
    }

    /**
     * Get the full path to a relative template file.
     *
     * @param relativeTemplateFile Template file
     * @return String Full template file path
     */
    @SuppressWarnings("UnstableApiUsage")
    @Override
    public String getFullTemplatePath(String relativeTemplateFile) {
        if (StringUtils.isNotEmpty(relativeTemplateFile)) {
            String loc = this.resourceLocation + File.separator + relativeTemplateFile;
            try {
                URL url = Resources.getResource(loc);
                if (url != null) {
                    return loc;
                }
            } catch (IllegalArgumentException e) {
                return null;
            }
        }
        return null;
    }
}
