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
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.tags.Tag;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.GlobalFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.meta.features.WireFormatFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.math.BigInteger;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RustSalvoServerCodegen extends AbstractRustCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "salvo-openapi-server";

    private String packageName;
    private String packageVersion;
    private Boolean enableRequestValidation = true;
    private Boolean enableResponseValidation = false;
    private Boolean enableAuthMiddleware = false;
    private Boolean enableCorsMiddleware = false;

    private String externCrateName;

    // Types specific to Salvo
    private static final String uuidType = "uuid::Uuid";
    private static final String bytesType = "Vec<u8>";
    private static final String dateType = "chrono::naive::NaiveDate";
    private static final String dateTimeType = "chrono::DateTime<chrono::Utc>";
    private static final String stringType = "String";
    private static final String objectType = "serde_json::Value";
    private static final String mapType = "std::collections::HashMap";
    private static final String vecType = "Vec";

    // MIME types
    private static final String jsonMimeType = "application/json";
    private static final String formUrlEncodedMimeType = "application/x-www-form-urlencoded";
    private static final String multipartMimeType = "multipart/form-data";
    private static final String plainTextMimeType = "text/plain";

    // Salvo-specific routing and handler mapping
    private final Map<String, ArrayList<SalvoOperation>> routeMap = new HashMap<>();
    private boolean hasAuthHandlers = false;

    private final Logger LOGGER = LoggerFactory.getLogger(RustSalvoServerCodegen.class);

    public RustSalvoServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .wireFormatFeatures(EnumSet.of(
                        WireFormatFeature.JSON,
                        WireFormatFeature.Custom
                ))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken
                ))
                .schemaSupportFeatures(EnumSet.of(
                        SchemaSupportFeature.Simple,
                        SchemaSupportFeature.Composite,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf,
                        SchemaSupportFeature.allOf
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.Info,
                        GlobalFeature.ExternalDocumentation,
                        GlobalFeature.Examples,
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.MultiServer,
                        GlobalFeature.ParameterizedServer,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        hideGenerationTimestamp = Boolean.FALSE;
        outputFolder = Path.of("generated-code", "rust-salvo").toString();
        embeddedTemplateDir = templateDir = "rust-salvo";

        importMapping = new HashMap<>();
        modelTemplateFiles.clear();
        apiTemplateFiles.put("handlers.mustache", ".rs");

        // Setup type mappings for Salvo
        setupTypeMappings();

        // Setup CLI options
        setupCliOptions();

        // Setup supporting files
        setupSupportingFiles();
    }

    private void setupTypeMappings() {
        defaultIncludes = new HashSet<>(Set.of("map", "array"));

        languageSpecificPrimitives = new HashSet<>(Set.of(
                "bool", "char", "i8", "i16", "i32", "i64",
                "u8", "u16", "u32", "u64", "isize", "usize",
                "f32", "f64", "str", stringType
        ));

        instantiationTypes = new HashMap<>(Map.of(
                "array", vecType,
                "map", mapType
        ));

        typeMapping = new HashMap<>(Map.ofEntries(
                new AbstractMap.SimpleEntry<>("number", "f64"),
                new AbstractMap.SimpleEntry<>("integer", "i32"),
                new AbstractMap.SimpleEntry<>("long", "i64"),
                new AbstractMap.SimpleEntry<>("float", "f32"),
                new AbstractMap.SimpleEntry<>("double", "f64"),
                new AbstractMap.SimpleEntry<>("string", stringType),
                new AbstractMap.SimpleEntry<>("UUID", uuidType),
                new AbstractMap.SimpleEntry<>("URI", stringType),
                new AbstractMap.SimpleEntry<>("byte", "u8"),
                new AbstractMap.SimpleEntry<>("ByteArray", bytesType),
                new AbstractMap.SimpleEntry<>("binary", bytesType),
                new AbstractMap.SimpleEntry<>("boolean", "bool"),
                new AbstractMap.SimpleEntry<>("date", dateType),
                new AbstractMap.SimpleEntry<>("DateTime", dateTimeType),
                new AbstractMap.SimpleEntry<>("password", stringType),
                new AbstractMap.SimpleEntry<>("File", bytesType),
                new AbstractMap.SimpleEntry<>("file", bytesType),
                new AbstractMap.SimpleEntry<>("array", vecType),
                new AbstractMap.SimpleEntry<>("map", mapType),
                new AbstractMap.SimpleEntry<>("object", objectType),
                new AbstractMap.SimpleEntry<>("AnyType", objectType)
        ));
    }

        private void setupCliOptions() {
        cliOptions = new ArrayList<>(List.of(
                new CliOption(CodegenConstants.PACKAGE_NAME,
                        "Rust crate name (convention: snake_case).")
                        .defaultValue("salvo_openapi"),
                new CliOption(CodegenConstants.PACKAGE_VERSION,
                        "Rust crate version."),
                new CliOption("enableRequestValidation",
                        "Enable request validation middleware")
                        .defaultValue(Boolean.TRUE.toString()),
                new CliOption("enableResponseValidation",
                        "Enable response validation middleware")
                        .defaultValue(Boolean.FALSE.toString()),
                new CliOption("enableAuthMiddleware",
                        "Enable authentication middleware")
                        .defaultValue(Boolean.FALSE.toString()),
                new CliOption("enableCorsMiddleware",
                        "Enable CORS middleware")
                        .defaultValue(Boolean.FALSE.toString())
        ));
    }

    private void setupSupportingFiles() {
        supportingFiles.add(new SupportingFile("Cargo.mustache", "", "Cargo.toml"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("main.mustache", "src", "main.rs"));
        supportingFiles.add(new SupportingFile("models.mustache", "src", "models.rs"));
        supportingFiles.add(new SupportingFile("handlers-mod.mustache", "src/handlers", "mod.rs"));
        supportingFiles.add(new SupportingFile("middleware.mustache", "src", "middleware.rs"));
        supportingFiles.add(new SupportingFile("routes.mustache", "src", "routes.rs"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md").doNotOverwrite());
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "rust-salvo";
    }

    @Override
    public String getHelp() {
        return "Generates a Rust server library using Salvo web framework.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        setPackageName((String) additionalProperties.getOrDefault(CodegenConstants.PACKAGE_NAME, "salvo_openapi"));

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }

        // Process Salvo-specific options; ensure defaults land in additionalProperties
        // so templates can branch on them and tests can inspect them.
        if (additionalProperties.containsKey("enableRequestValidation")) {
            enableRequestValidation = convertPropertyToBooleanAndWriteBack("enableRequestValidation");
        } else {
            additionalProperties.put("enableRequestValidation", enableRequestValidation);
        }

        if (additionalProperties.containsKey("enableResponseValidation")) {
            enableResponseValidation = convertPropertyToBooleanAndWriteBack("enableResponseValidation");
        } else {
            additionalProperties.put("enableResponseValidation", enableResponseValidation);
        }

        if (additionalProperties.containsKey("enableAuthMiddleware")) {
            enableAuthMiddleware = convertPropertyToBooleanAndWriteBack("enableAuthMiddleware");
        } else {
            additionalProperties.put("enableAuthMiddleware", enableAuthMiddleware);
        }

        if (additionalProperties.containsKey("enableCorsMiddleware")) {
            enableCorsMiddleware = convertPropertyToBooleanAndWriteBack("enableCorsMiddleware");
        } else {
            additionalProperties.put("enableCorsMiddleware", enableCorsMiddleware);
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put("externCrateName", externCrateName);
    }

    private void setPackageName(String packageName) {
        this.packageName = packageName;
        this.externCrateName = packageName.replace('-', '_');
    }

    private void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    @Override
    public String apiPackage() {
        return "src" + File.separator + "handlers";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();

        if (packageVersion == null || packageVersion.isEmpty()) {
            List<String> versionComponents = new ArrayList<>(Arrays.asList(info.getVersion().split("[.]")));
            if (versionComponents.isEmpty()) {
                versionComponents.add("1");
            }
            while (versionComponents.size() < 3) {
                versionComponents.add("0");
            }
            setPackageVersion(String.join(".", versionComponents));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
    }

    @Override
    public String toApiName(String name) {
        return name.isEmpty() ?
                "default" :
                sanitizeIdentifier(name, CasingType.SNAKE_CASE, "api", "API", true);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String apiFileFolder() {
        return Path.of(outputFolder, apiPackage().replace('.', File.separatorChar)).toString();
    }

    @Override
    public String toOperationId(String operationId) {
        return sanitizeIdentifier(operationId, CasingType.SNAKE_CASE, "handler", "handler", true);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        // Salvo-specific operation processing
        String handlerName = underscore(op.operationId);
        String method = httpMethod.toLowerCase(Locale.ROOT);
        op.vendorExtensions.put("x-handler-name", handlerName);
        op.vendorExtensions.put("x-route-path", convertPathToSalvoFormat(path));
        op.vendorExtensions.put("x-http-method", method);

        // Group operations by route for Salvo router setup. AuthSchemes is
        // populated later in postProcessOperationsWithModels because
        // CodegenOperation.authMethods is set by DefaultGenerator AFTER
        // fromOperation returns.
        String salvoPath = convertPathToSalvoFormat(path);
        routeMap.computeIfAbsent(salvoPath, k -> new ArrayList<>())
                .add(new SalvoOperation(method, handlerName, op.vendorExtensions, Collections.emptyList()));

        return op;
    }

    private String convertPathToSalvoFormat(String path) {
        // Salvo 0.76+ uses {name} for path parameters, matching OpenAPI.
        return path;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operationsMap, List<ModelMap> allModels) {
        OperationMap operations = operationsMap.getOperations();
        operations.put("classnamePascalCase", camelize(operations.getClassname()));

        for (CodegenOperation op : operations.getOperation()) {
            List<SalvoAuthScheme> opAuthSchemes = new ArrayList<>();
            if (op.authMethods != null && !op.authMethods.isEmpty()) {
                LinkedHashSet<String> seen = new LinkedHashSet<>();
                for (CodegenSecurity sec : op.authMethods) {
                    SalvoAuthScheme s = SalvoAuthScheme.fromCodegen(sec);
                    if (s != null && seen.add(s.kind)) {
                        opAuthSchemes.add(s);
                    }
                }
            }

            boolean opHasAuth = !opAuthSchemes.isEmpty();
            if (opHasAuth) {
                hasAuthHandlers = true;
                op.vendorExtensions.put("x-has-auth", true);
                op.vendorExtensions.put("x-salvo-auth-schemes", opAuthSchemes);
                op.vendorExtensions.put("x-salvo-auth-multi", opAuthSchemes.size() > 1);
            }

            // Mirror the schemes onto the matching SalvoOperation so routes.mustache
            // can attach per-route hoops without having to flatten vendorExtensions.
            String handlerName = (String) op.vendorExtensions.get("x-handler-name");
            String method = (String) op.vendorExtensions.get("x-http-method");
            if (handlerName != null && method != null) {
                for (ArrayList<SalvoOperation> group : routeMap.values()) {
                    for (SalvoOperation so : group) {
                        if (method.equals(so.method) && handlerName.equals(so.handlerName)) {
                            so.authSchemes = opAuthSchemes;
                            so.hasAuth = opHasAuth;
                            so.multiAuth = opAuthSchemes.size() > 1;
                        }
                    }
                }
            }
        }

        if (hasAuthHandlers) {
            operations.put("hasAuthHandlers", true);
            operations.getOperation().forEach(op -> op.vendorExtensions.put("hasAuthHandlers", true));
        }

        return operationsMap;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {
        generateYAMLSpecFile(bundle);

        // Add Salvo-specific routing information
        List<SalvoRouteGroup> routeGroups = routeMap.entrySet().stream()
                .map(entry -> new SalvoRouteGroup(entry.getKey(), entry.getValue()))
                .sorted(Comparator.comparing(group -> group.path))
                .collect(Collectors.toList());

        bundle.put("salvoRoutes", routeGroups);
        bundle.put("hasAuthHandlers", hasAuthHandlers);
        bundle.put("enableRequestValidation", enableRequestValidation);
        bundle.put("enableResponseValidation", enableResponseValidation);
        bundle.put("enableAuthMiddleware", enableAuthMiddleware);
        bundle.put("enableCorsMiddleware", enableCorsMiddleware);

        return super.postProcessSupportingFileData(bundle);
    }



    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }

        String fileName = file.toString();
        String cmd = System.getenv("RUST_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(cmd)) {
            cmd = "rustfmt";
        }

        if ("rs".equals(FilenameUtils.getExtension(fileName))) {
            executePostProcessor(new String[]{cmd, "--edition", "2021", fileName});
        }
    }

    // Helper classes for Salvo-specific data structures
    static class SalvoOperation {
        public String method;
        public String handlerName;
        public Map<String, Object> vendorExtensions;
        public List<SalvoAuthScheme> authSchemes;
        public boolean hasAuth;
        public boolean multiAuth;

        SalvoOperation(String method, String handlerName, Map<String, Object> vendorExtensions,
                       List<SalvoAuthScheme> authSchemes) {
            this.method = method;
            this.handlerName = handlerName;
            this.vendorExtensions = vendorExtensions;
            this.authSchemes = authSchemes == null ? Collections.emptyList() : authSchemes;
            this.hasAuth = !this.authSchemes.isEmpty();
            this.multiAuth = this.authSchemes.size() > 1;
        }
    }

    static class SalvoRouteGroup {
        public String path;
        public ArrayList<SalvoOperation> operations;

        SalvoRouteGroup(String path, ArrayList<SalvoOperation> operations) {
            this.path = path;
            this.operations = operations;
        }
    }

    // Auth scheme descriptor exposed to templates. `kind` is used to pick the
    // factory function in routes.mustache; `schemaName` shows up in the
    // `#[salvo::oapi::endpoint(security(...))]` annotation along with any
    // declared OAuth2/OIDC scopes.
    static class SalvoAuthScheme {
        public String kind;        // "apiKey" | "basic" | "bearer" | "oauth2"
        public String factory;     // factory fn in middleware.rs, null for oauth2
        public String schemaName;  // name used in OpenAPI security() annotation
        public List<SalvoAuthScope> scopes;

        SalvoAuthScheme(String kind, String factory, String schemaName, List<SalvoAuthScope> scopes) {
            this.kind = kind;
            this.factory = factory;
            this.schemaName = schemaName;
            this.scopes = scopes;
        }

        static SalvoAuthScheme fromCodegen(CodegenSecurity sec) {
            List<SalvoAuthScope> scopeList = extractScopes(sec);
            if (Boolean.TRUE.equals(sec.isApiKey)) {
                return new SalvoAuthScheme("apiKey", "api_key_auth", "ApiKeyAuth", scopeList);
            }
            if (Boolean.TRUE.equals(sec.isBasic) && Boolean.TRUE.equals(sec.isBasicBearer)) {
                return new SalvoAuthScheme("bearer", "bearer_auth", "BearerAuth", scopeList);
            }
            if (Boolean.TRUE.equals(sec.isBasic) && Boolean.TRUE.equals(sec.isBasicBasic)) {
                return new SalvoAuthScheme("basic", "basic_auth_hoop", "BasicAuth", scopeList);
            }
            if (Boolean.TRUE.equals(sec.isBasic)) {
                return new SalvoAuthScheme("basic", "basic_auth_hoop", "BasicAuth", scopeList);
            }
            // OAuth2 / OpenIdConnect: leave runtime enforcement to the user,
            // but surface the scheme + scopes so the OpenAPI annotation stays
            // faithful to the source spec.
            if (Boolean.TRUE.equals(sec.isOAuth)) {
                return new SalvoAuthScheme("oauth2", null, "OAuth2", scopeList);
            }
            return null;
        }

        @SuppressWarnings("unchecked")
        private static List<SalvoAuthScope> extractScopes(CodegenSecurity sec) {
            List<SalvoAuthScope> out = new ArrayList<>();
            if (sec.scopes == null) {
                return out;
            }
            for (Map<String, Object> entry : sec.scopes) {
                Object name = entry.get("scope");
                if (name != null) {
                    out.add(new SalvoAuthScope(name.toString()));
                }
            }
            return out;
        }
    }

    // Single OAuth2/OIDC scope name. Held in its own class so mustache can
    // render the comma-separated list naturally with `{{#-last}}` semantics.
    static class SalvoAuthScope {
        public String scope;

        SalvoAuthScope(String scope) {
            this.scope = scope;
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        // Surface a serde rename whenever the Rust field name (already snake_cased
        // and keyword-escaped by AbstractRustCodegen.toVarName) differs from the
        // OpenAPI wire name, so JSON contracts stay intact (e.g. `petId` on the
        // wire, `pet_id` in Rust).
        if (property.baseName != null && property.name != null
                && !property.name.equals(property.baseName)) {
            property.vendorExtensions.put("x-salvo-serde-rename", true);
        }
    }
}
