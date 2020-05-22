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
            String loc = "_common" + File.separator + relativeTemplateFile;
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
