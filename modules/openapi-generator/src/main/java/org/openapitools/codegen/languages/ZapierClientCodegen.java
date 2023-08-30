/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.languages;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.escape;

public class ZapierClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";

    private final Logger LOGGER = LoggerFactory.getLogger(ZapierClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "zapier";
    }

    public String getHelp() {
        return "Generates a zapier client.";
    }

    public ZapierClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "zapier";
        modelTemplateFiles.put("model.mustache", ".js");
        apiTemplateFiles.put("api.mustache", ".js");
        embeddedTemplateDir = templateDir = "zapier";
        apiPackage = "apis";
        testPackage = "samples";
        modelPackage = "models";
        apiTestTemplateFiles.put("sample.mustache", ".js");
        supportingFiles.add(new SupportingFile("actions.mustache", "operations", "actions.js"));
        supportingFiles.add(new SupportingFile("utils.mustache", "utils", "utils.js"));
        supportingFiles.add(new SupportingFile("index.mustache", "", "index.js"));
        supportingFiles.add(new SupportingFile("authentication.mustache", "", "authentication.js"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("number", "integer", "string", "boolean", "array", "file", "object")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("set", "array");
        instantiationTypes.put("list", "array");
        instantiationTypes.put("map", "object");
        typeMapping = new HashMap<>();
        typeMapping.put("array", "array");
        typeMapping.put("set", "array");
        typeMapping.put("map", "object");
        typeMapping.put("List", "array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "integer");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("decimal", "number");
        typeMapping.put("DateTime", "string");
        typeMapping.put("date", "string");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "object");
        typeMapping.put("integer", "integer");
        typeMapping.put("binary", "file");
        typeMapping.put("file", "file");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
    }

    @Override
    protected void initializeSpecialCharacterMapping() {
        super.initializeSpecialCharacterMapping();
        specialCharReplacements.remove("_");
    }

    /**
     * Works identically to {@link DefaultCodegen#toParamName(String)} but doesn't camelize.
     *
     * @param name Codegen property object
     * @return the sanitized parameter name
     */
    @Override
    public String toParamName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains(String.valueOf((char) character)))) {
            return escape(name, specialCharReplacements, null, null);
        }
        return name;
    }

    @Override
    public String toModelName(final String name) {
        return name;
    }

    @Override
    public String toModelImport(String name) {
        return "const " + name + " = " + "require('../" + modelPackage() + "/" + name + "');";
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (!needToImport(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }
        if (null == type) {
            LOGGER.error("No Type defined for Schema {}", p);
        }
        return toModelName(type);
    }

    @Override
    public String toModelFilename(String name) {
        return name;
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name);
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return null;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input;
    }

    @Override
    public CodegenResponse fromResponse(String responseCode, ApiResponse response) {
        CodegenResponse r = super.fromResponse(responseCode, response);
        try {
            Map<String, Map<String, Map<String, Object>>> map = Json.mapper().readerFor(Map.class).readValue(Json.pretty(response.getContent()));
            Map.Entry<String, Map<String, Map<String, Object>>> entry = map.entrySet().stream().findFirst().orElseThrow(()-> new IllegalStateException("no response object available"));
            Map<String, Map<String, Object>> example = entry.getValue();
            r.examples = toExamples(example.get("examples"));
        } catch (Exception e) {
            LOGGER.debug(e.toString());
        }
        return r;
    }

    @Override
    protected List<Map<String, Object>> toExamples(Map<String, Object> examples) {
        if (examples == null) {
            return null;
        }

        final List<Map<String, Object>> output = new ArrayList<>(examples.size());
        for (Map.Entry<String, Object> entry : examples.entrySet()) {
            final Map<String, Object> kv = new HashMap<>();
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) entry.getValue();
            String example = "";
            try {
                example = Json.mapper().writeValueAsString(map.getOrDefault("value", map));
            } catch (Exception ignored) {
            }

            kv.put("example", example);
            output.add(kv);
        }

        return output;
    }

}
