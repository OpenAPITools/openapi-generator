/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.api;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Locale;
import java.util.Map;

/**
 * Each templating engine is called by an Adapter, selected at runtime
 */
public interface TemplatingEngineAdapter {

    /**
     * Provides an identifier used to load the adapter. This could be a name, uuid, or any other string.
     *
     * @return A string identifier.
     */
    String getIdentifier();

    /**
     * During generation, if a supporting file has a file extension that is
     * inside that array, then it is considered a templated supporting file
     * and we use the templating engine adapter to generate it
     *
     * @return string array of the valid file extensions for this templating engine
     */
    String[] getFileExtensions();

    /**
     * Determine if the adapter handles compilation of the file
     * @param filename The template filename
     *
     * @return True if the file should be compiled by this adapter, else false.
     */
    default boolean handlesFile(String filename) {
        return filename != null && filename.length() > 0 && Arrays.stream(getFileExtensions()).anyMatch(i -> filename.endsWith("." + i));
    }

    /**
     * Compiles a template into a string
     *
     * @param executor    From where we can fetch the templates content (e.g. an instance of DefaultGenerator)
     * @param bundle       The map of values to pass to the template
     * @param templateFile The name of the template (e.g. model.mustache )
     * @return the processed template result
     * @throws IOException an error occurred in the template processing
     */
    String compileTemplate(TemplatingExecutor executor, Map<String, Object> bundle,
                           String templateFile) throws IOException;

    /**
     * Determines whether the template file with supported extensions exists. This may be on the filesystem,
     * external filesystem, or classpath (implementation is up to TemplatingGenerator).
     *
     * @param generator    The generator holding details about file resolution
     * @param templateFile The original target filename
     * @return True if the template is available in the template search path, false if it can not be found
     */
    @SuppressWarnings({"java:S2093"}) // ignore java:S2093 because we have double-assignment to the closeable
    default boolean templateExists(TemplatingExecutor generator, String templateFile) {
        return Arrays.stream(getFileExtensions()).anyMatch(ext -> {
            int idx = templateFile.lastIndexOf('.');
            String baseName;
            if (idx > 0 && idx < templateFile.length() - 1) {
                baseName = templateFile.substring(0, idx);
            } else {
                baseName = templateFile;
            }

            Path path = generator.getFullTemplatePath(String.format(Locale.ROOT, "%s.%s", baseName, ext));

            InputStream is = null;
            try {
                String resourcePath = System.getProperty("os.name").startsWith("Windows") ?
                        path.toString().replace("\\", "/") :
                        path.toString();
                is = this.getClass().getClassLoader().getResourceAsStream(resourcePath);
                if (is == null) {
                    is = new FileInputStream(path.toFile());
                }

                return is.available() > 0;
            } catch (IOException e) {
                // ignore
            } finally {
                try {
                    if (is != null) is.close();
                } catch (IOException e) {
                    // ignore
                }
            }
            return false;
        });
    }
}
