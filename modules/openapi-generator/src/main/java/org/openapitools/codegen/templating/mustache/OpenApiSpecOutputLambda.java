/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.templating.mustache;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.OpenAPI;
import org.openapitools.codegen.serializer.SerializerUtils;

import java.io.IOException;
import java.io.Writer;

/**
 * Writes the OpenAPI document to template output without materializing the full
 * YAML/JSON document as a string first.
 */
public class OpenApiSpecOutputLambda implements Mustache.Lambda {
    private final OpenAPI openAPI;
    private final Format format;
    private final boolean sortOutput;

    public OpenApiSpecOutputLambda(OpenAPI openAPI, Format format) {
        this(openAPI, format, false);
    }

    public OpenApiSpecOutputLambda(OpenAPI openAPI, Format format, boolean sortOutput) {
        this.openAPI = openAPI;
        this.format = format;
        this.sortOutput = sortOutput;
    }

    @Override
    public void execute(Template.Fragment fragment, Writer writer) throws IOException {
        if (format == Format.JSON) {
            SerializerUtils.writeJson(openAPI, writer, sortOutput);
        } else {
            SerializerUtils.writeYaml(openAPI, writer, sortOutput);
        }
    }

    public enum Format {
        JSON,
        YAML
    }
}
