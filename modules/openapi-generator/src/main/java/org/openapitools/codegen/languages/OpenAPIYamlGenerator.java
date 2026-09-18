/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache.Lambda;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.serializer.SerializerUtils;
import org.openapitools.codegen.templating.mustache.OnChangeLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.OpenAPISorter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

public class OpenAPIYamlGenerator extends DefaultCodegen implements CodegenConfig {
    public static final String OUTPUT_NAME = "outputFile";
    public static final String SORT_OUTPUT = "sortOutput";

    private final Logger LOGGER = LoggerFactory.getLogger(OpenAPIYamlGenerator.class);

    protected String outputFile = "openapi/openapi.yaml";
    protected boolean sortOutput = false;

    public OpenAPIYamlGenerator() {
        super();

        modifyFeatureSet(features -> features
                .documentationFeatures(EnumSet.allOf(DocumentationFeature.class))
                .dataTypeFeatures(EnumSet.allOf(DataTypeFeature.class))
                .wireFormatFeatures(EnumSet.allOf(WireFormatFeature.class))
                .securityFeatures(EnumSet.allOf(SecurityFeature.class))
                .globalFeatures(EnumSet.allOf(GlobalFeature.class))
                .parameterFeatures(EnumSet.allOf(ParameterFeature.class))
                .schemaSupportFeatures(EnumSet.allOf(SchemaSupportFeature.class))
        );

        embeddedTemplateDir = templateDir = "openapi-yaml";
        outputFolder = "generated-code/openapi-yaml";

        cliOptions.clear();
        cliOptions.add(CliOption.newString(OUTPUT_NAME, "Output filename").defaultValue(outputFile));
        cliOptions.add(CliOption.newBoolean(SORT_OUTPUT,
                "Sort paths alphabetically, component maps (schemas, parameters, requestBodies, responses, " +
                        "headers, examples, links, callbacks, securitySchemes) by name, and HTTP methods in classical " +
                        "order (GET, PUT, POST, DELETE, OPTIONS, HEAD, PATCH, TRACE).")
                .defaultValue(Boolean.FALSE.toString()));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "openapi-yaml";
    }

    @Override
    public String getHelp() {
        return "Creates a static openapi.yaml file (OpenAPI spec v3).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(OUTPUT_NAME)) {
            outputFile = additionalProperties.get(OUTPUT_NAME).toString();
        }
        LOGGER.info("Output file [outputFile={}]", outputFile);
        supportingFiles.add(new SupportingFile("openapi.mustache", outputFile));

        if (additionalProperties.containsKey(SORT_OUTPUT)) {
            sortOutput = Boolean.parseBoolean(additionalProperties.get(SORT_OUTPUT).toString());
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        // swagger-parser can leave nested multi-file $refs as relative paths into the root
        // document (e.g. ./swagger.yml#/components/schemas/ComplexType). Rewrite those to
        // internal refs when the target already exists in local components so the bundled
        // openapi.yaml is self-contained and valid. See #24528.
        // Run in preprocessOpenAPI (before models / info processing) rather than processOpenAPI.
        localizeExternalComponentRefs(openAPI);
        super.preprocessOpenAPI(openAPI);
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
        if (sortOutput) {
            OpenAPISorter.sort(openAPI);
        }
    }

    /**
     * Rewrite external-looking component $refs that still point at a local component to
     * internal {@code #/components/...} form.
     *
     * @param openAPI OpenAPI document being processed
     */
    void localizeExternalComponentRefs(OpenAPI openAPI) {
        if (openAPI == null || openAPI.getComponents() == null
                || openAPI.getComponents().getSchemas() == null) {
            return;
        }
        // Walk component schemas directly so we do not call getSimpleRef (and its warnings)
        // on still-external nested refs before they are rewritten.
        for (Schema<?> schema : openAPI.getComponents().getSchemas().values()) {
            walkAndRewriteSchemaRefs(schema, openAPI);
        }
    }

    private void walkAndRewriteSchemaRefs(Schema<?> schema, OpenAPI openAPI) {
        if (schema == null) {
            return;
        }
        rewriteSchemaRefIfLocal(schema, openAPI);
        if (schema.getProperties() != null) {
            for (Object property : schema.getProperties().values()) {
                walkAndRewriteSchemaRefs((Schema<?>) property, openAPI);
            }
        }
        if (ModelUtils.isArraySchema(schema) && schema.getItems() != null) {
            walkAndRewriteSchemaRefs(schema.getItems(), openAPI);
        }
        if (schema.getAdditionalProperties() instanceof Schema) {
            walkAndRewriteSchemaRefs((Schema<?>) schema.getAdditionalProperties(), openAPI);
        }
        if (schema.getAllOf() != null) {
            for (Object s : schema.getAllOf()) {
                walkAndRewriteSchemaRefs((Schema<?>) s, openAPI);
            }
        }
        if (schema.getAnyOf() != null) {
            for (Object s : schema.getAnyOf()) {
                walkAndRewriteSchemaRefs((Schema<?>) s, openAPI);
            }
        }
        if (schema.getOneOf() != null) {
            for (Object s : schema.getOneOf()) {
                walkAndRewriteSchemaRefs((Schema<?>) s, openAPI);
            }
        }
        if (schema.getNot() != null) {
            walkAndRewriteSchemaRefs(schema.getNot(), openAPI);
        }
    }

    private void rewriteSchemaRefIfLocal(Schema<?> schema, OpenAPI openAPI) {
        if (schema == null || StringUtils.isEmpty(schema.get$ref())) {
            return;
        }
        String localRef = toLocalComponentRef(schema.get$ref(), openAPI);
        if (localRef != null) {
            schema.set$ref(localRef);
        }
    }

    /**
     * If {@code ref} is an external (file-relative or absolute) reference into a local
     * component, return the internal {@code #/components/...} form; otherwise return null.
     *
     * @param ref     original $ref value
     * @param openAPI OpenAPI document used to verify the target exists locally
     * @return internal ref, or null when no rewrite should be applied
     */
    String toLocalComponentRef(String ref, OpenAPI openAPI) {
        if (ref == null || ref.startsWith("#/")) {
            return null;
        }
        int fragmentIndex = ref.indexOf("#/components/");
        if (fragmentIndex < 0) {
            return null;
        }
        String fragment = ref.substring(fragmentIndex);
        // fragment is "#/components/{section}/{name}[/...]" — strip "#/" before splitting
        String path = fragment.startsWith("#/") ? fragment.substring(2) : fragment.substring(1);
        String[] parts = path.split("/");
        // parts[0]=components, parts[1]=section, parts[2]=name
        if (parts.length < 3 || !"components".equals(parts[0])) {
            return null;
        }
        String section = parts[1];
        String name = URLDecoder.decode(parts[2], StandardCharsets.UTF_8)
                .replace("~1", "/")
                .replace("~0", "~");
        if (!componentExists(openAPI.getComponents(), section, name)) {
            return null;
        }
        return fragment;
    }

    private static boolean componentExists(Components components, String section, String name) {
        if (components == null || StringUtils.isEmpty(name)) {
            return false;
        }
        switch (section) {
            case "schemas":
                return components.getSchemas() != null && components.getSchemas().containsKey(name);
            case "parameters":
                return components.getParameters() != null && components.getParameters().containsKey(name);
            case "responses":
                return components.getResponses() != null && components.getResponses().containsKey(name);
            case "requestBodies":
                return components.getRequestBodies() != null && components.getRequestBodies().containsKey(name);
            case "headers":
                return components.getHeaders() != null && components.getHeaders().containsKey(name);
            case "examples":
                return components.getExamples() != null && components.getExamples().containsKey(name);
            case "links":
                return components.getLinks() != null && components.getLinks().containsKey(name);
            case "callbacks":
                return components.getCallbacks() != null && components.getCallbacks().containsKey(name);
            case "securitySchemes":
                return components.getSecuritySchemes() != null && components.getSecuritySchemes().containsKey(name);
            default:
                return false;
        }
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("onchange", new OnChangeLambda());
    }

    /**
     * Group operations by resourcePath so that operations with same path and
     * different http method can be rendered one after the other.
     */
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.computeIfAbsent(resourcePath,
                k -> new ArrayList<>());
        opList.add(co);
    }

    @Override
    public void generateYAMLSpecFile(Map<String, Object> objs) {
        OpenAPI openAPI = (OpenAPI) objs.get("openAPI");
        String yaml = SerializerUtils.toYamlString(openAPI, sortOutput);
        if (yaml != null) {
            objs.put("openapi-yaml", yaml);
        }
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // just return the original string
        return input;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // just return the original string
        return input;
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return null;
    }
}
