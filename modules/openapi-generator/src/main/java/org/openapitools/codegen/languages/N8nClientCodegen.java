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

import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ApiInfoMap;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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

    /** Tracks which raw OpenAPI tag first claimed each sanitized Resource name, so a second,
     * different raw tag that happens to sanitize to the same value can be disambiguated
     * instead of silently merging into the first tag's Resource. See {@link #sanitizeTag}. */
    private final Map<String, String> sanitizedTagOwners = new HashMap<>();

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

        // credentials.mustache is deliberately NOT registered as a fixed-destination
        // SupportingFile: a spec can declare more than one security scheme, and each needs its
        // own file/class pair (n8n's directory loader derives the expected class name from the
        // filename - see the Javadoc on writeCredentialFile()), which the SupportingFile
        // mechanism's one-template-one-fixed-path model can't produce. Instead,
        // postProcessSupportingFileData() below renders this template manually, once per
        // declared scheme.
        supportingFiles.add(new SupportingFile("node.mustache", "nodes" + File.separator + "Api", "Api.node.ts"));
        supportingFiles.add(new SupportingFile("genericFunctions.mustache", "nodes" + File.separator + "Api", "GenericFunctions.ts"));
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
     * Disambiguates two distinct raw OpenAPI tags that sanitize/normalize to the same Resource
     * label (e.g. "Pet Store" and "pet_store" both camelizing to "PetStore") by appending a
     * deterministic suffix to every tag after the first one to claim that sanitized value, per
     * FR-011 / spec.md Edge Cases #3. Deterministic on the raw tag string, so repeated calls for
     * the same colliding tag (one per operation under it) keep returning the same disambiguated
     * value, letting those operations still group together under one Resource.
     */
    @Override
    public String sanitizeTag(String tag) {
        String sanitized = super.sanitizeTag(tag);
        String existingOwner = sanitizedTagOwners.putIfAbsent(sanitized, tag);
        if (existingOwner == null || existingOwner.equals(tag)) {
            return sanitized;
        }
        return sanitized + "_" + Integer.toHexString(tag.hashCode());
    }

    /**
     * Operations with no OpenAPI tag at all are, by default, grouped under the framework's
     * literal fallback tag {@code "default"} (see {@code DefaultGenerator.processOperation}) -
     * meaning every untagged operation across the whole spec would be lumped into one single
     * "Default" Resource. spec.md US2 Acceptance Scenario 3 requires them instead to be grouped
     * into a Resource derived from the operation's own path, so untagged operations under
     * different paths become different Resources rather than one catch-all.
     */
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        if (operation.getTags() == null || operation.getTags().isEmpty()) {
            tag = sanitizeTag(pathDerivedResourceName(resourcePath));
        }
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
    }

    private static String pathDerivedResourceName(String resourcePath) {
        if (resourcePath != null) {
            for (String segment : resourcePath.split("/")) {
                if (!segment.isEmpty() && !segment.startsWith("{")) {
                    return segment;
                }
            }
        }
        return "default";
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

    /**
     * Renders one {@code <Scheme>Credentials.credentials.ts} file per declared security scheme
     * that this generator knows how to map (apiKey, HTTP basic, HTTP bearer, OAuth2 - per
     * research.md §3), and exposes the resulting list as {@code n8nCredentialRefs} /
     * {@code n8nSingleCredential} so {@code node.mustache}, {@code package.mustache}, {@code
     * genericFunctions.mustache}, and {@code README.mustache} (all rendered from this same
     * bundle) can reference every generated credential, not just one. See FR-005's edge case
     * ("generate credentials for each declared scheme") and
     * contracts/generated-package-contract.md's one-credential-per-scheme design.
     * <p>
     * Also stamps each individual {@code CodegenOperation} with {@code
     * vendorExtensions.n8nOperationCredentialName} (T052 / spec.md Edge Cases #4, 2nd clause:
     * "let the generated node reference the one appropriate per operation"): looked up from
     * that operation's own, already-framework-filtered {@code authMethods} when it declares a
     * security requirement of its own, falling back to the single highest-priority generated
     * credential otherwise. This has to happen here, in this hook, rather than in {@link
     * #postProcessOperationsWithModels} - at that point {@code op.authMethods} has not been set
     * yet; {@code DefaultGenerator} only calls {@code addAuthenticationSwitches(operation)}
     * afterward. By the time this hook runs (during {@code buildSupportFileBundle}, after
     * {@code generateApis()} has fully finished), every operation's {@code authMethods} is
     * already populated.
     */
    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs = super.postProcessSupportingFileData(objs);

        List<CodegenSecurity> authMethods = (List<CodegenSecurity>) objs.get("authMethods");
        List<Map<String, Object>> credentialRefs = new ArrayList<>();
        Map<String, String> schemeNameToCredentialName = new HashMap<>();

        if (authMethods != null && !authMethods.isEmpty()) {
            List<CodegenSecurity> supported = new ArrayList<>();
            for (CodegenSecurity scheme : authMethods) {
                if (schemePriority(scheme) != Integer.MAX_VALUE) {
                    supported.add(scheme);
                }
            }
            supported.sort(Comparator.comparingInt(N8nClientCodegen::schemePriority));

            String credentialsTemplate = readClasspathResource("n8n/credentials.mustache");
            Mustache.Compiler compiler = Mustache.compiler().defaultValue("");

            for (CodegenSecurity scheme : supported) {
                String pascal = toPascalCase(scheme.name);
                String className = pascal + "Credentials";
                String credentialName = Character.toLowerCase(pascal.charAt(0)) + pascal.substring(1) + "Credentials";
                String displayName = objs.get("apiTitle") + " API - " + schemeKindLabel(scheme);

                Map<String, Object> data = new HashMap<>(objs);
                data.put("className", className);
                data.put("credentialName", credentialName);
                data.put("displayName", displayName);
                data.put("isApiKey", scheme.isApiKey);
                data.put("isBasicBasic", scheme.isBasicBasic);
                data.put("isBasicBearer", scheme.isBasicBearer);
                data.put("isOAuth", scheme.isOAuth);
                data.put("isKeyInHeader", scheme.isKeyInHeader);
                data.put("isKeyInQuery", scheme.isKeyInQuery);
                data.put("keyParamName", scheme.keyParamName);

                String rendered = compiler.compile(credentialsTemplate).execute(data);
                writeCredentialFile(className, rendered);

                Map<String, Object> ref = new HashMap<>();
                ref.put("className", className);
                ref.put("credentialName", credentialName);
                ref.put("displayName", displayName);
                credentialRefs.add(ref);

                schemeNameToCredentialName.put(scheme.name, credentialName);
            }
        }

        boolean single = credentialRefs.size() == 1;
        for (Map<String, Object> ref : credentialRefs) {
            ref.put("n8nSingleCredential", single);
        }

        objs.put("n8nCredentialRefs", credentialRefs);
        objs.put("n8nSingleCredential", single);

        String primaryCredentialName = credentialRefs.isEmpty() ? "" : (String) credentialRefs.get(0).get("credentialName");
        applyPerOperationCredentialNames(objs, schemeNameToCredentialName, primaryCredentialName);

        return objs;
    }

    @SuppressWarnings("unchecked")
    private static void applyPerOperationCredentialNames(Map<String, Object> objs, Map<String, String> schemeNameToCredentialName, String primaryCredentialName) {
        Object apiInfo = objs.get("apiInfo");
        if (!(apiInfo instanceof ApiInfoMap)) {
            return;
        }
        List<OperationsMap> apis = ((ApiInfoMap) apiInfo).getApis();
        if (apis == null) {
            return;
        }
        for (OperationsMap api : apis) {
            List<CodegenOperation> operations = api.getOperations().getOperation();
            for (CodegenOperation op : operations) {
                String chosen = primaryCredentialName;
                if (op.authMethods != null) {
                    for (CodegenSecurity scheme : op.authMethods) {
                        String mapped = schemeNameToCredentialName.get(scheme.name);
                        if (mapped != null) {
                            chosen = mapped;
                            break;
                        }
                    }
                }
                op.vendorExtensions.put("n8nOperationCredentialName", chosen);
            }
        }
    }

    private static int schemePriority(CodegenSecurity scheme) {
        if (Boolean.TRUE.equals(scheme.isApiKey)) {
            return 0;
        }
        if (Boolean.TRUE.equals(scheme.isBasicBasic)) {
            return 1;
        }
        if (Boolean.TRUE.equals(scheme.isBasicBearer)) {
            return 2;
        }
        if (Boolean.TRUE.equals(scheme.isOAuth)) {
            return 3;
        }
        // openIdConnect, httpSignature, or any other scheme type this generator doesn't map:
        // no credential is generated for it (documented as a limitation elsewhere), per FR-010.
        return Integer.MAX_VALUE;
    }

    private static String schemeKindLabel(CodegenSecurity scheme) {
        if (Boolean.TRUE.equals(scheme.isApiKey)) {
            return "API Key";
        }
        if (Boolean.TRUE.equals(scheme.isBasicBasic)) {
            return "HTTP Basic Auth";
        }
        if (Boolean.TRUE.equals(scheme.isBasicBearer)) {
            return "Bearer Token";
        }
        if (Boolean.TRUE.equals(scheme.isOAuth)) {
            return "OAuth2";
        }
        return "API";
    }

    private static String toPascalCase(String raw) {
        String[] parts = raw.split("[^a-zA-Z0-9]+");
        StringBuilder sb = new StringBuilder();
        for (String part : parts) {
            if (!part.isEmpty()) {
                sb.append(Character.toUpperCase(part.charAt(0))).append(part.substring(1));
            }
        }
        if (sb.length() == 0 || Character.isDigit(sb.charAt(0))) {
            sb.insert(0, "Scheme");
        }
        return sb.toString();
    }

    private String readClasspathResource(String path) {
        try (InputStream in = getClass().getClassLoader().getResourceAsStream(path)) {
            if (in == null) {
                throw new IllegalStateException("Could not find classpath resource: " + path);
            }
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException("Failed to read classpath resource: " + path, e);
        }
    }

    /**
     * n8n's {@code CustomDirectoryLoader}/{@code DirectoryLoader.loadClass} derives the expected
     * exported class name directly from the filename (splitting on "." and taking the first
     * segment), so {@code <className>.credentials.ts} MUST export a class literally named
     * {@code <className>} - a mismatch is silently swallowed as a logged warning rather than a
     * hard error, and the credential then fails at runtime with "Unknown credential name"
     * (confirmed by actually loading generated output into a real n8n instance).
     */
    private void writeCredentialFile(String className, String content) {
        try {
            Path dir = Paths.get(outputFolder(), "credentials");
            Files.createDirectories(dir);
            Files.write(dir.resolve(className + ".credentials.ts"), content.getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new UncheckedIOException("Failed to write generated credential file for class " + className, e);
        }
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
