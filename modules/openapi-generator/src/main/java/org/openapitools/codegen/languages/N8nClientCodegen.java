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
import org.openapitools.codegen.config.GlobalSettings;
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
 * overall approach as {@link ZapierClientCodegen}: a thin {@code DefaultCodegen} subclass.
 * Templates still lean on the framework's existing per-tag operation grouping
 * ({@code apiInfo.apis}) and auto-populated security-scheme bundle variables
 * ({@code apiKeyMethods}, {@code httpBasicMethods}, {@code httpBearerMethods},
 * {@code oauthMethods}) wherever they already produce the right shape, but this class does
 * override {@link #sanitizeTag} and {@link #addOperationToGroup} to fix up Resource grouping
 * (path-derived fallback for untagged operations, disambiguation of colliding sanitized tag
 * names) and {@link #postProcessSupportingFileData} to generate one credential per declared
 * security scheme and resolve each operation's own matching credential - see those methods'
 * own Javadoc for why the framework's defaults weren't sufficient as-is.
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

        // node.mustache, genericFunctions.mustache, and credentials.mustache are deliberately
        // NOT registered as fixed-destination SupportingFiles: their class/file names need to
        // incorporate nodePrefix (so two packages generated from two different specs don't
        // produce byte-identical node type names / class names - T061), and nodePrefix isn't
        // known until the spec itself is parsed (preprocessOpenAPI), well after the
        // SupportingFile list is built here in the constructor. A spec can also declare more
        // than one security scheme, needing one credentials file/class pair each (n8n's
        // directory loader derives the expected class name from the filename - see the Javadoc
        // on writeGeneratedFile()). Both needs are beyond the SupportingFile mechanism's
        // one-template-one-fixed-path model, so postProcessSupportingFileData() below renders
        // all three templates manually instead.
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

    /**
     * Arrays get their own {@code collection} n8nType (data-model.md "Field", T007/T062),
     * distinct from the {@code json} fallback: FR-006 requires "array" to map to an
     * "appropriate" node field type, and spec.md Edge Cases #2 reserves the generic-JSON
     * fallback specifically for shapes the generator "cannot cleanly map" (its own example is
     * deeply-nested {@code oneOf}/{@code anyOf}) - a plain array of strings or numbers (e.g.
     * Petstore's own {@code findPetsByStatus}/{@code findPetsByTags} query parameters) is not
     * such a shape and, before this, was rendered as an opaque free-text JSON field in the n8n
     * UI even though n8n's {@code collection} type (with {@code multipleValues: true}) can
     * represent "add one or more values" natively. Maps/free-form objects still fall back to
     * {@code json}: an arbitrary key/value bag has no fixed set of sub-fields to build a
     * {@code collection}'s {@code options} array from.
     * <p>
     * Checked before {@code isEnum}: the framework sets a parameter's own {@code isEnum} true
     * whenever its array *items* are enum-typed (confirmed empirically against Petstore's own
     * {@code status}/{@code tags} query parameters, both arrays of enum strings) - checking
     * {@code isEnum} first would collapse an array-of-enum down to a single-select dropdown,
     * silently dropping its "more than one value" array-ness. The per-item enum (if any) is
     * instead carried into the {@code collection}'s own per-entry sub-field via {@code
     * n8nCollectionItemIsOptions}/{@code n8nCollectionItemAllowableValues} below.
     * <p>
     * Also computes {@code vendorExtensions.n8nDefaultLiteral} (T067 - data-model.md "Field"
     * {@code default}: "OpenAPI {@code default} - Pre-fills the n8n field"): a complete,
     * ready-to-embed JS literal for the field's {@code default:} property, built from the
     * parameter's own spec-declared {@code defaultValue} rather than always falling back to a
     * generic type-based filler (confirmed empirically: before this, a boolean param declared
     * {@code default: true} rendered {@code default: false}, and an enum param declared
     * {@code default: published} - not its first choice - rendered {@code default: "draft"}
     * instead). Falls back to the previous generic filler only when the spec declares no
     * default for that field (or, for {@code options}, when the declared default doesn't match
     * any enum choice).
     */
    private static void applyN8nFieldType(CodegenParameter p) {
        // See cleanDefaultValue's Javadoc: fromParameter/fromFormProperty-built parameters go
        // through toDefaultParameterValue, which carries the same "null"-string sentinel as
        // CodegenProperty.defaultValue - confirmed empirically against the committed Petstore
        // sample, a plain path parameter with no declared default (petId) rendered
        // `default: null` before this normalization, not the generic numeric filler.
        p.defaultValue = cleanDefaultValue(p.defaultValue);
        String n8nFieldType;
        if (Boolean.TRUE.equals(p.isArray)) {
            n8nFieldType = "collection";
        } else if (Boolean.TRUE.equals(p.isEnum)) {
            n8nFieldType = "options";
        } else if (Boolean.TRUE.equals(p.isMap) || Boolean.TRUE.equals(p.isFreeFormObject)) {
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
        p.vendorExtensions.put("n8nIsCollection", "collection".equals(n8nFieldType));

        String defaultLiteral;
        switch (n8nFieldType) {
            case "boolean":
                defaultLiteral = p.defaultValue != null ? p.defaultValue : "false";
                break;
            case "number":
                defaultLiteral = p.defaultValue != null ? p.defaultValue : "0";
                break;
            case "string":
                defaultLiteral = "'" + escapeSingleQuotedJs(p.defaultValue != null ? p.defaultValue : "") + "'";
                break;
            case "options":
                defaultLiteral = enumDefaultLiteral(p.allowableValues, p.defaultValue);
                break;
            case "collection":
                defaultLiteral = "[]";
                break;
            default:
                // json fallback: no spec-level concept of a scalar default to pre-fill here.
                defaultLiteral = "{}";
        }
        p.vendorExtensions.put("n8nDefaultLiteral", defaultLiteral);

        if ("collection".equals(n8nFieldType)) {
            CodegenProperty items = p.items;
            boolean itemIsOptions = items != null && Boolean.TRUE.equals(items.isEnum);
            String itemFieldType;
            if (itemIsOptions) {
                itemFieldType = "options";
            } else if (items != null && "boolean".equals(items.dataType)) {
                itemFieldType = "boolean";
            } else if (items != null && ("integer".equals(items.dataType) || "number".equals(items.dataType))) {
                itemFieldType = "number";
            } else {
                itemFieldType = "string";
            }
            p.vendorExtensions.put("n8nCollectionItemType", itemFieldType);
            p.vendorExtensions.put("n8nCollectionItemIsBoolean", "boolean".equals(itemFieldType));
            p.vendorExtensions.put("n8nCollectionItemIsNumber", "number".equals(itemFieldType));
            p.vendorExtensions.put("n8nCollectionItemIsString", "string".equals(itemFieldType));
            p.vendorExtensions.put("n8nCollectionItemIsOptions", itemIsOptions);
            if (itemIsOptions) {
                p.vendorExtensions.put("n8nCollectionItemAllowableValues", items.allowableValues);
            }

            String itemDefault = items != null ? cleanDefaultValue(items.defaultValue) : null;
            String itemDefaultLiteral;
            if (itemIsOptions) {
                itemDefaultLiteral = enumDefaultLiteral(items.allowableValues, itemDefault);
            } else if ("boolean".equals(itemFieldType)) {
                itemDefaultLiteral = itemDefault != null ? itemDefault : "false";
            } else if ("number".equals(itemFieldType)) {
                itemDefaultLiteral = itemDefault != null ? itemDefault : "0";
            } else {
                itemDefaultLiteral = "'" + escapeSingleQuotedJs(itemDefault != null ? itemDefault : "") + "'";
            }
            p.vendorExtensions.put("n8nCollectionItemDefaultLiteral", itemDefaultLiteral);
        }
    }

    /**
     * Computes {@code nodeClassName}/{@code nodeTypeName} from {@code nodePrefix} (T061 -
     * `{{nodePrefix}}Api`, lowercased-first for the internal n8n type name), so two packages
     * generated from two different specs - each with their own spec-derived nodePrefix - don't
     * produce the byte-identical node class/file names and `INodeTypeDescription.name` that
     * used to make one of them silently fail registration in n8n when both were installed into
     * the same instance (confirmed via the same loader mechanism that caught the earlier
     * filename/classname bug for credentials). Then renders one {@code
     * <nodePrefix><Scheme>Credentials.credentials.ts} file per declared security scheme that
     * this generator knows how to map (apiKey, HTTP basic, HTTP bearer, OAuth2 - per
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
     * that operation's own, already-framework-filtered {@code authMethods}, which the
     * framework itself already resolves correctly - the operation's own declared scheme, or
     * the spec's global default when it declares none of its own, or {@code null} when neither
     * applies (including an explicit {@code security: []} opt-out). An operation with no
     * resolvable scheme gets an empty credential name (T056 - see {@link
     * #applyPerOperationCredentialNames}'s own Javadoc for why no fallback is applied). This
     * has to happen here, in this hook, rather than in {@link
     * #postProcessOperationsWithModels} - at that point {@code op.authMethods} has not been set
     * yet; {@code DefaultGenerator} only calls {@code addAuthenticationSwitches(operation)}
     * afterward. By the time this hook runs (during {@code buildSupportFileBundle}, after
     * {@code generateApis()} has fully finished), every operation's {@code authMethods} is
     * already populated. Finally renders {@code node.mustache}/{@code genericFunctions.mustache}
     * from this fully-built bundle, last, so they see {@code nodeClassName}/{@code
     * nodeTypeName}/{@code n8nCredentialRefs}/every operation's {@code n8nOperationCredentialName}.
     */
    @Override
    @SuppressWarnings("unchecked")
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        objs = super.postProcessSupportingFileData(objs);

        Object nodePrefixValue = objs.get(NODE_PREFIX);
        String nodePrefix = nodePrefixValue != null ? nodePrefixValue.toString() : "Generated";
        String nodeClassName = nodePrefix + "Api";
        String nodeTypeName = lowerFirst(nodeClassName);
        objs.put("nodeClassName", nodeClassName);
        objs.put("nodeTypeName", nodeTypeName);

        Mustache.Compiler compiler = Mustache.compiler().defaultValue("");

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

            // The framework emits one CodegenSecurity per declared OAuth2 flow, all sharing the
            // same scheme name, when a single securityScheme declares more than one flow at
            // once (DefaultCodegen#fromSecurity) - grouped back here by name so each distinct
            // scheme still gets exactly one credential (contracts/generated-package-contract.md),
            // and so T063 below can tell "exactly one flow" apart from "multiple simultaneous
            // flows" for a given scheme.
            Map<String, List<CodegenSecurity>> flowsByScheme = new LinkedHashMap<>();
            for (CodegenSecurity scheme : supported) {
                flowsByScheme.computeIfAbsent(scheme.name, key -> new ArrayList<>()).add(scheme);
            }

            String credentialsTemplate = readClasspathResource("n8n/credentials.mustache");

            for (Map.Entry<String, List<CodegenSecurity>> schemeFlows : flowsByScheme.entrySet()) {
                List<CodegenSecurity> flows = schemeFlows.getValue();
                CodegenSecurity scheme = flows.get(0);
                boolean singleFlow = flows.size() == 1;

                // Prefixed with nodePrefix (T061), same as nodeClassName, so a credential
                // declared in one generated package never collides with one of the same
                // scheme name from a different generated package installed alongside it.
                String pascal = toPascalCase(scheme.name);
                String className = nodePrefix + pascal + "Credentials";
                String credentialName = lowerFirst(className);
                String displayName = objs.get("apiTitle") + " API - " + schemeKindLabel(scheme);

                // n8n's generic OAuth2 credential type (`extends: ['oAuth2Api']`) is only
                // auto-wired for the "common flows" research.md §3 / spec.md Assumptions commit
                // to automating - clientCredentials ("application") and authorizationCode
                // ("code") - and only when the scheme declares exactly one flow; any other flow
                // (implicit, password), or an ambiguous multi-flow scheme, keeps the
                // manual-paste-a-token fallback instead of guessing which flow to perform (T063).
                boolean oauth2AutoWired = singleFlow && Boolean.TRUE.equals(scheme.isOAuth)
                        && (Boolean.TRUE.equals(scheme.isApplication) || Boolean.TRUE.equals(scheme.isCode));
                boolean oauth2Manual = Boolean.TRUE.equals(scheme.isOAuth) && !oauth2AutoWired;

                Map<String, Object> data = new HashMap<>(objs);
                data.put("className", className);
                data.put("credentialName", credentialName);
                data.put("displayName", displayName);
                data.put("isApiKey", scheme.isApiKey);
                data.put("isBasicBasic", scheme.isBasicBasic);
                data.put("isBasicBearer", scheme.isBasicBearer);
                data.put("isOAuthAutoWired", oauth2AutoWired);
                data.put("isOAuthManual", oauth2Manual);
                data.put("oauth2GrantType", Boolean.TRUE.equals(scheme.isApplication) ? "clientCredentials" : "authorizationCode");
                data.put("oauth2AuthUrl", scheme.authorizationUrl);
                data.put("oauth2TokenUrl", scheme.tokenUrl);
                data.put("oauth2Scopes", joinScopeNames(scheme.scopes));
                data.put("isKeyInHeader", scheme.isKeyInHeader);
                data.put("isKeyInQuery", scheme.isKeyInQuery);
                data.put("keyParamName", scheme.keyParamName);

                String rendered = compiler.compile(credentialsTemplate).execute(data);
                writeGeneratedFile("credentials" + File.separator + className + ".credentials.ts", rendered);

                Map<String, Object> ref = new HashMap<>();
                ref.put("className", className);
                ref.put("credentialName", credentialName);
                ref.put("displayName", displayName);
                ref.put("isOAuthAutoWired", oauth2AutoWired);
                ref.put("isOAuthManual", oauth2Manual);
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

        // Distinguishes "the spec declares no security at all" from "the spec declares a
        // security scheme this generator cannot map" (T065 - e.g. openIdConnect, httpSignature,
        // or any other scheme landing in schemePriority()'s unsupported/MAX_VALUE bucket):
        // without this, README.mustache's `{{^n8nCredentialRefs}}` branch rendered the same
        // "no security scheme declared" message for both cases, which is actively false - and
        // silently hides the real limitation - when a scheme WAS declared but just isn't one
        // this generator knows how to translate, per FR-007/FR-010/spec.md US3 Acceptance
        // Scenario 3.
        List<String> unmappedSchemeNames = new ArrayList<>();
        if (authMethods != null) {
            Set<String> seen = new LinkedHashSet<>();
            for (CodegenSecurity scheme : authMethods) {
                if (!schemeNameToCredentialName.containsKey(scheme.name)) {
                    seen.add(scheme.name);
                }
            }
            unmappedSchemeNames.addAll(seen);
        }
        objs.put("n8nUnmappedSchemeNames", unmappedSchemeNames);
        objs.put("n8nHasUnmappedSchemes", !unmappedSchemeNames.isEmpty());

        applyPerOperationCredentialNames(objs, schemeNameToCredentialName);

        // node.mustache/genericFunctions.mustache are rendered from this same, now-fully-built
        // bundle (apiInfo, n8nCredentialRefs, nodeClassName/nodeTypeName, every operation's
        // n8nOperationCredentialName) - see the constructor's comment on why these can't be
        // fixed-destination SupportingFiles.
        String nodeRendered = compiler.compile(readClasspathResource("n8n/node.mustache")).execute(objs);
        String genericFunctionsRendered = compiler.compile(readClasspathResource("n8n/genericFunctions.mustache")).execute(objs);
        writeGeneratedFile("nodes" + File.separator + nodeClassName + File.separator + nodeClassName + ".node.ts", nodeRendered);
        writeGeneratedFile("nodes" + File.separator + nodeClassName + File.separator + "GenericFunctions.ts", genericFunctionsRendered);

        return objs;
    }

    private static String lowerFirst(String value) {
        if (value == null || value.isEmpty()) {
            return value;
        }
        return Character.toLowerCase(value.charAt(0)) + value.substring(1);
    }

    /**
     * {@code CodegenProperty.defaultValue} (unlike {@code CodegenParameter.defaultValue}) is the
     * literal string {@code "null"} - not Java {@code null} - when the schema declares no
     * default ({@code DefaultCodegen#toDefaultValue}'s documented sentinel for "no default").
     * Confirmed empirically: without this normalization, Petstore's {@code addPet}/{@code id}
     * property rendered {@code default: null} (a bare, confusing literal) and its {@code name}
     * property rendered {@code default: 'null'} (the literal text "null" pre-filled into a
     * string field) instead of falling back to the generic filler like every other
     * no-default field already does.
     */
    private static String cleanDefaultValue(String rawDefaultValue) {
        return (rawDefaultValue == null || "null".equals(rawDefaultValue)) ? null : rawDefaultValue;
    }

    private static String escapeSingleQuotedJs(String value) {
        return value.replace("\\", "\\\\").replace("'", "\\'");
    }

    /**
     * Finds the enum choice matching a declared OpenAPI default, returning its already-escaped
     * {@code value} literal (e.g. {@code "published"} for a string enum) rather than re-quoting
     * it. Falls back to the first choice's literal when there's no declared default or it
     * doesn't match any enum choice - the previous, only behavior (T067).
     */
    @SuppressWarnings("unchecked")
    private static String enumDefaultLiteral(Map<String, Object> allowableValues, String declaredDefault) {
        if (allowableValues == null || !(allowableValues.get("enumVars") instanceof List)) {
            return "''";
        }
        List<Map<String, Object>> enumVars = (List<Map<String, Object>>) allowableValues.get("enumVars");
        if (enumVars.isEmpty()) {
            return "''";
        }
        if (declaredDefault != null) {
            for (Map<String, Object> enumVar : enumVars) {
                Object rawValue = enumVar.get("rawValue");
                if (rawValue != null && declaredDefault.equals(String.valueOf(rawValue))) {
                    Object value = enumVar.get("value");
                    if (value != null) {
                        return String.valueOf(value);
                    }
                }
            }
        }
        Object firstValue = enumVars.get(0).get("value");
        return firstValue != null ? String.valueOf(firstValue) : "''";
    }

    /**
     * {@code CodegenOperation.authMethods} is {@code null} for exactly two cases that must be
     * treated identically here: an operation with no {@code security:} key of its own (and no
     * spec-wide default to inherit), and one that explicitly opts out with {@code security: []}
     * (per OpenAPI semantics - see {@code DefaultGenerator.processOperation}'s {@code
     * if (securities != null && securities.isEmpty()) continue;}). Both mean "this operation
     * genuinely needs no authentication" - confirmed against Petstore's own spec, where
     * {@code placeOrder}/{@code getOrderById}/{@code getUserByName}/{@code loginUser}/{@code
     * deleteOrder} declare no security at all and the live API accepts them unauthenticated.
     * An earlier version of this method fell back to the single highest-priority generated
     * credential in this case, which forced authentication onto operations the spec never
     * asked to be authenticated - fixed by leaving {@code n8nOperationCredentialName} empty
     * instead, which {@code genericFunctions.mustache}'s {@code apiRequest} already treats as
     * "send unauthenticated" (falsy {@code credentialName} argument).
     */
    @SuppressWarnings("unchecked")
    private static void applyPerOperationCredentialNames(Map<String, Object> objs, Map<String, String> schemeNameToCredentialName) {
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
                String chosen = "";
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

    /** Joins an OAuth2 scheme's declared scopes into a single space-separated string, the
     * format n8n's generic {@code oAuth2Api} credential's {@code scope} field expects. */
    private static String joinScopeNames(List<Map<String, Object>> scopes) {
        if (scopes == null || scopes.isEmpty()) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for (Map<String, Object> scope : scopes) {
            Object name = scope.get("scope");
            if (name != null) {
                if (sb.length() > 0) {
                    sb.append(' ');
                }
                sb.append(name);
            }
        }
        return sb.toString();
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
     * For credential files specifically: n8n's {@code CustomDirectoryLoader}/{@code
     * DirectoryLoader.loadClass} derives the expected exported class name directly from the
     * filename (splitting on "." and taking the first segment), so {@code
     * <className>.credentials.ts} MUST export a class literally named {@code <className>} - a
     * mismatch is silently swallowed as a logged warning rather than a hard error, and the
     * credential then fails at runtime with "Unknown credential name" (confirmed by actually
     * loading generated output into a real n8n instance).
     * <p>
     * This writes directly via {@link Files#write}, bypassing the framework's normal
     * {@code SupportingFile}/{@code templateProcessor} pipeline (which can't express either
     * "one file per dynamically-discovered item", for credentials, or "destination depends on
     * spec content resolved after the SupportingFile list is built", for the node/
     * GenericFunctions files - see the constructor's comment and {@link
     * #postProcessSupportingFileData}'s Javadoc). That pipeline is also what the framework's
     * {@code --dry-run} mode intercepts, so this method has to check {@code GlobalSettings}'
     * {@code dryRun} flag itself (confirmed by actually running {@code generate ... --dry-run}:
     * without this check, these files were written to disk for real despite dry-run, and
     * didn't even appear in the dry-run report).
     *
     * @param relativePath path under the output folder, e.g. {@code "credentials/X.credentials.ts"}
     *                      or {@code "nodes/Y/Y.node.ts"}
     */
    private void writeGeneratedFile(String relativePath, String content) {
        if (Boolean.parseBoolean(GlobalSettings.getProperty("dryRun"))) {
            return;
        }
        try {
            Path file = Paths.get(outputFolder(), relativePath);
            Files.createDirectories(file.getParent());
            Files.write(file, content.getBytes(StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new UncheckedIOException("Failed to write generated file: " + relativePath, e);
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
