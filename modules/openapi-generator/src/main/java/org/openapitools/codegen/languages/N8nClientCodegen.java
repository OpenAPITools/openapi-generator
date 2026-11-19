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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.escape;

/**
 * <p>Mustache templates are located in {@code src/main/resources/n8n/}.
 * <p>Generates a partial n8n community node package (node definition, credentials
 * definition, package manifest, README) from an OpenAPI spec, following the same
 * overall approach as {@link ZapierClientCodegen}: a thin {@code DefaultCodegen}
 * subclass whose templates lean on the framework's existing per-tag operation
 * grouping ({@code apiInfo.apis}) and auto-populated security-scheme bundle
 * variables ({@code apiKeyMethods}, {@code httpBasicMethods}, {@code httpBearerMethods},
 * {@code oauthMethods}) rather than re-implementing that grouping/mapping logic here.
 */
public class N8nClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String NODE_PREFIX = "nodePrefix";

    private final Logger LOGGER = LoggerFactory.getLogger(N8nClientCodegen.class);

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "n8n";
    }

    @Override
    public String getHelp() {
        return "Generates an n8n community node package (node definition, credentials, package.json, README) from an OpenAPI spec.";
    }

    public N8nClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "n8n";
        embeddedTemplateDir = templateDir = "n8n";

        // n8n's output is a small, fixed set of files (node + credentials + package
        // manifest + helpers + README), not one file per model/tag, so no
        // modelTemplateFiles/apiTemplateFiles are registered; everything is emitted
        // via supportingFiles, with node.mustache looping over the framework's own
        // apiInfo.apis/operations structure for Resource/Operation grouping.
        apiPackage = "nodes";
        modelPackage = "nodes";

        supportingFiles.add(new SupportingFile("node.mustache", "nodes" + File.separator + "Api", "Api.node.ts"));
        supportingFiles.add(new SupportingFile("genericFunctions.mustache", "nodes" + File.separator + "Api", "GenericFunctions.ts"));
        supportingFiles.add(new SupportingFile("credentials.mustache", "credentials", "ApiCredentials.credentials.ts"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", "", "tsconfig.json"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        cliOptions.add(new CliOption(NPM_NAME, "The name of the generated npm package (defaults to a sanitized form of the spec's info.title)."));
        cliOptions.add(new CliOption(NPM_VERSION, "The version of the generated npm package (defaults to the spec's info.version, or 0.1.0).").defaultValue("0.1.0"));
        cliOptions.add(new CliOption(NODE_PREFIX, "Prefix applied to the generated n8n node/credential display names, to avoid collisions when several generated nodes are installed together."));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("number", "integer", "string", "boolean", "array", "file", "object")
        );

        instantiationTypes.put("array", "array");
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
     * Works identically to {@link DefaultCodegen#toParamName(String)} but doesn't camelize,
     * matching {@link ZapierClientCodegen#toParamName(String)}: the generated field/param
     * names are used as JS/TS object keys shown directly in the n8n UI, not as idiomatic
     * camelCase identifiers.
     */
    @Override
    public String toParamName(String name) {
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.containsKey(String.valueOf((char) character)))) {
            return escape(name, specialCharReplacements, null, null);
        }
        return name;
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.TYPESCRIPT;
    }

    /**
     * Computes the n8n node-property {@code type} for each parameter/field (data-model.md
     * "Field"/{@code n8nType}), exposed to templates as {@code vendorExtensions.n8nFieldType}.
     * <p>
     * This is done here, against {@code dataType}, rather than via the parameter's
     * {@code isNumeric}/{@code isBoolean}/{@code isString} flags: those flags are only reliably
     * populated for a subset of schema/format combinations (confirmed empirically - e.g. a plain
     * {@code integer}/{@code int64} path parameter leaves {@code isNumeric} false), so branching
     * on them directly from the mustache template mis-detects some primitive parameters as the
     * generic JSON fallback.
     * <p>
     * Applied once per operation, after all of its parameters (path/query/header, request
     * body, and form-expanded fields alike - these are built via several different internal
     * {@code DefaultCodegen} code paths) have been assembled into {@code allParams}. Doing
     * it here, rather than by overriding each individual {@code fromParameter}/
     * {@code fromRequestBody}-style builder method, guarantees every parameter that actually
     * reaches the {@code node.mustache} template has an {@code n8nFieldType} set, regardless
     * of which internal path constructed it.
     */
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        for (CodegenOperation op : objs.getOperations().getOperation()) {
            for (CodegenParameter p : op.allParams) {
                applyN8nFieldType(p);
            }
        }
        return objs;
    }

    private static void applyN8nFieldType(CodegenParameter p) {
        String n8nFieldType;
        if (Boolean.TRUE.equals(p.isEnum)) {
            n8nFieldType = "options";
        } else if (Boolean.TRUE.equals(p.isArray) || Boolean.TRUE.equals(p.isMap) || Boolean.TRUE.equals(p.isFreeFormObject)) {
            n8nFieldType = "json";
        } else if ("boolean".equals(p.dataType)) {
            n8nFieldType = "boolean";
        } else if ("integer".equals(p.dataType) || "number".equals(p.dataType)) {
            n8nFieldType = "number";
        } else if ("string".equals(p.dataType)) {
            n8nFieldType = "string";
        } else {
            // complex/model types (object refs, oneOf/anyOf, unmapped schemas): generic JSON
            // fallback, per FR-006 / data-model.md Field.n8nType.
            n8nFieldType = "json";
        }

        p.vendorExtensions.put("n8nFieldType", n8nFieldType);
        p.vendorExtensions.put("n8nIsOptions", "options".equals(n8nFieldType));
        p.vendorExtensions.put("n8nIsBoolean", "boolean".equals(n8nFieldType));
        p.vendorExtensions.put("n8nIsNumber", "number".equals(n8nFieldType));
        p.vendorExtensions.put("n8nIsString", "string".equals(n8nFieldType));
        p.vendorExtensions.put("n8nIsJson", "json".equals(n8nFieldType));
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "\\\"");
    }

    /**
     * Defaults npmName/npmVersion/nodePrefix from the spec's own info block when the user
     * hasn't supplied them via CLI/config, per contracts/generator-cli-options.md. This is
     * done here (rather than in {@code processOpts()}) because {@code info.title}/{@code
     * info.version} aren't available until the OpenAPI document itself has been parsed.
     */
    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        Info info = openAPI.getInfo();

        if (!additionalProperties.containsKey(NPM_NAME)) {
            String title = (info != null && info.getTitle() != null) ? info.getTitle() : "n8n-api-client";
            additionalProperties.put(NPM_NAME, sanitizeNpmName(title));
        }

        if (!additionalProperties.containsKey(NPM_VERSION)) {
            String version = (info != null && info.getVersion() != null) ? info.getVersion() : "0.1.0";
            additionalProperties.put(NPM_VERSION, version);
        }

        if (!additionalProperties.containsKey(NODE_PREFIX)) {
            additionalProperties.put(NODE_PREFIX, capitalize(sanitizeNpmName((String) additionalProperties.get(NPM_NAME))));
        }

        if (info != null && info.getTitle() != null) {
            additionalProperties.put("apiTitle", info.getTitle());
        } else {
            additionalProperties.put("apiTitle", additionalProperties.get(NPM_NAME));
        }
    }

    private static String sanitizeNpmName(String title) {
        String sanitized = title.trim().toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9-]+", "-").replaceAll("^-+|-+$", "");
        return sanitized.isEmpty() ? "n8n-api-client" : sanitized;
    }

    private static String capitalize(String value) {
        if (value == null || value.isEmpty()) {
            return value;
        }
        String[] parts = value.split("-");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (!part.isEmpty()) {
                sb.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
            }
        }
        return sb.toString();
    }
}
