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

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import joptsimple.internal.Strings;
import lombok.Setter;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.math.BigInteger;
import java.net.URL;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RustServerCodegen extends AbstractRustCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(RustServerCodegen.class);

    private Map<String, String> modelXmlNames = new HashMap<String, String>();

    protected String apiVersion = "1.0.0";
    protected String serverHost = "localhost";
    protected int serverPort = 8080;
    protected String projectName = "openapi-server";
    protected String apiPath = "rust-server";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String packageName;
    @Setter protected String packageVersion;
    protected String externCrateName;
    protected Map<String, Map<String, String>> pathSetMap = new HashMap();
    protected Map<String, Map<String, String>> callbacksPathSetMap = new HashMap();

    private static final String uuidType = "uuid::Uuid";
    private static final String bytesType = "swagger::ByteArray";

    private static final String xmlMimeType = "application/xml";
    private static final String textXmlMimeType = "text/xml";
    private static final String octetMimeType = "application/octet-stream";
    private static final String plainTextMimeType = "text/plain";
    private static final String jsonMimeType = "application/json";

    // RFC 7386 support
    private static final String mergePatchJsonMimeType = "application/merge-patch+json";

    // RFC 7807 Support
    private static final String problemJsonMimeType = "application/problem+json";
    private static final String problemXmlMimeType = "application/problem+xml";

    public RustServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath
                )
        );

        // We need inline enums to be resolved to a separate model so that
        // anyOf/oneOf that contain them work correctly.
        inlineSchemaOption.put("RESOLVE_INLINE_ENUMS", "true");

        // Show the generation timestamp by default
        hideGenerationTimestamp = Boolean.FALSE;

        // set the output folder here
        outputFolder = "generated-code/rust-server";

        /*
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles.clear();

        /*
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.clear();

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "rust-server";

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "char",
                        "i8",
                        "i16",
                        "i32",
                        "i64",
                        "u8",
                        "u16",
                        "u32",
                        "u64",
                        "isize",
                        "usize",
                        "f32",
                        "f64",
                        "str",
                        "String")
        );

        instantiationTypes.clear();
        instantiationTypes.put("array", "Vec");
        instantiationTypes.put("map", "std::collections::HashMap");

        typeMapping.clear();
        typeMapping.put("number", "f64");
        typeMapping.put("integer", "i32");
        typeMapping.put("long", "i64");
        typeMapping.put("float", "f32");
        typeMapping.put("double", "f64");
        typeMapping.put("string", "String");
        typeMapping.put("UUID", uuidType);
        typeMapping.put("URI", "String");
        typeMapping.put("byte", "u8");
        typeMapping.put("ByteArray", bytesType);
        typeMapping.put("binary", bytesType);
        typeMapping.put("boolean", "bool");
        typeMapping.put("date", "chrono::naive::NaiveDate");
        typeMapping.put("DateTime", "chrono::DateTime::<chrono::Utc>");
        typeMapping.put("password", "String");
        typeMapping.put("File", bytesType);
        typeMapping.put("file", bytesType);
        typeMapping.put("array", "Vec");
        typeMapping.put("map", "std::collections::HashMap");
        typeMapping.put("object", "serde_json::Value");
        typeMapping.put("AnyType", "serde_json::Value");

        importMapping = new HashMap<String, String>();

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME,
                "Rust crate name (convention: snake_case).")
                .defaultValue("openapi_client"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION,
                "Rust crate version."));

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties.put("apiPath", apiPath);

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("Cargo.mustache", "", "Cargo.toml"));
        supportingFiles.add(new SupportingFile("cargo-config", ".cargo", "config.toml"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("context.mustache", "src", "context.rs"));
        supportingFiles.add(new SupportingFile("models.mustache", "src", "models.rs"));
        supportingFiles.add(new SupportingFile("header.mustache", "src", "header.rs"));
        supportingFiles.add(new SupportingFile("auth.mustache", "src", "auth.rs"));
        supportingFiles.add(new SupportingFile("server-mod.mustache", "src/server", "mod.rs"));
        supportingFiles.add(new SupportingFile("server-server_auth.mustache", "src/server", "server_auth.rs"));
        supportingFiles.add(new SupportingFile("client-mod.mustache", "src/client", "mod.rs"));
        supportingFiles.add(new SupportingFile("example-server-main.mustache", "examples/server", "main.rs"));
        supportingFiles.add(new SupportingFile("example-server-server.mustache", "examples/server", "server.rs"));
        supportingFiles.add(new SupportingFile("example-server-auth.mustache", "examples/server", "server_auth.rs"));
        supportingFiles.add(new SupportingFile("example-client-main.mustache", "examples/client", "main.rs"));
        supportingFiles.add(new SupportingFile("example-client-auth.mustache", "examples/client", "client_auth.rs"));
        supportingFiles.add(new SupportingFile("example-ca.pem", "examples", "ca.pem"));
        supportingFiles.add(new SupportingFile("example-server-chain.pem", "examples", "server-chain.pem"));
        supportingFiles.add(new SupportingFile("example-server-key.pem", "examples", "server-key.pem"));
        supportingFiles.add(new SupportingFile("bin-cli.mustache", "bin", "cli.rs"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("RUST_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable RUST_POST_PROCESS_FILE not defined. rustfmt will be used" +
                    " by default. To choose a different tool, try" +
                    " 'export RUST_POST_PROCESS_FILE=\"/usr/local/bin/rustfmt\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` " +
                    " (--enable-post-process-file for CLI).");
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'RUST_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (!Boolean.TRUE.equals(ModelUtils.isGenerateAliasAsModel())) {
            LOGGER.warn("generateAliasAsModel is set to false, which means array/map will be generated as model instead and the resulting code may have issues. Please enable `generateAliasAsModel` to address the issue.");
        }

        setPackageName((String) additionalProperties.getOrDefault(CodegenConstants.PACKAGE_NAME, "openapi_client"));

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put("externCrateName", externCrateName);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;

        // Also set the extern crate name, which has any '-' replace with a '_'.
        this.externCrateName = packageName.replace('-', '_');
    }

    @Override
    public String apiPackage() {
        return apiPath;
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "rust-server";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Rust Hyper/Tower server library. Also generates a matching Hyper client library within " +
                "the same crate that implements the same trait.";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {

        Info info = openAPI.getInfo();

        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        additionalProperties.put("serverHost", url.getHost());
        additionalProperties.put("serverPort", URLPathUtils.getPort(url, serverPort));

        if (packageVersion == null || packageVersion.isEmpty()) {
            List<String> versionComponents = new ArrayList<>(Arrays.asList(info.getVersion().split("[.]")));
            if (versionComponents.size() < 1) {
                versionComponents.add("1");
            }
            while (versionComponents.size() < 3) {
                versionComponents.add("0");
            }

            setPackageVersion(StringUtils.join(versionComponents, "."));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
    }

    @Override
    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "default";
        }
        return sanitizeIdentifier(name, CasingType.SNAKE_CASE, "api", "API", true);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toOperationId(String operationId) {
        // rust-server uses camel case instead
        return sanitizeIdentifier(operationId, CasingType.CAMEL_CASE, "call", "method", true);
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        // rust-server templates expect value to be in quotes
        return "\"" + super.toEnumValue(value, datatype) + "\"";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name) + "_api";
    }

    private boolean isMimetypeXml(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(xmlMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(problemXmlMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(textXmlMimeType);
    }

    private boolean isMimetypeJson(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(jsonMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(mergePatchJsonMimeType) ||
                mimetype.toLowerCase(Locale.ROOT).startsWith(problemJsonMimeType);
    }

    private boolean isMimetypeWwwFormUrlEncoded(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("application/x-www-form-urlencoded");
    }

    private boolean isMimetypeMultipartFormData(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("multipart/form-data");
    }

    private boolean isMimetypeOctetStream(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(octetMimeType);
    }

    private boolean isMimetypeMultipartRelated(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("multipart/related");
    }

    private boolean isMimetypeUnknown(String mimetype) {
        return "*/*".equals(mimetype);
    }

    /**
     * Do we have any special handling for this mimetype?
     */
    boolean isMimetypePlain(String mimetype) {
        boolean result = !(isMimetypeUnknown(mimetype) ||
                isMimetypeXml(mimetype) ||
                isMimetypeJson(mimetype) ||
                isMimetypeWwwFormUrlEncoded(mimetype) ||
                isMimetypeMultipartFormData(mimetype) ||
                isMimetypeMultipartRelated(mimetype));
        return result;
    }

    private String tidyUpRuntimeCallbackParam(String param) {
        return underscore(param.replace("-", "_").replace(".", "_").replace("{", "").replace("#", "_").replace("/", "_").replace("}", "").replace("$", "").replaceAll("_+", "_"));
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        Map<String, Schema> definitions = ModelUtils.getSchemas(this.openAPI);
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        String pathFormatString = op.path;
        for (CodegenParameter param : op.pathParams) {
            // Replace {baseName} with {paramName} for format string
            String paramSearch = "{" + param.baseName + "}";
            String paramReplace = "{" + param.paramName + "}";

            pathFormatString = pathFormatString.replace(paramSearch, paramReplace);
        }
        op.vendorExtensions.put("x-path-format-string", pathFormatString);

        // The Rust code will need to contain a series of regular expressions.
        // For performance, we'll construct these at start-of-day and re-use
        // them.  That means we need labels for them.
        //
        // Construct a Rust constant (uppercase) token name, and ensure it's
        // unique using a numeric tie-breaker if required.
        String basePathId = sanitizeName(op.path.replace("/", "_").replace("{", "").replace("}", "").replaceAll("^_", "")).toUpperCase(Locale.ROOT);
        String pathId = basePathId;
        int pathIdTiebreaker = 2;
        boolean found = false;

        Map<String, Map<String, String>> pathSetMap;

        // The callback API is logically distinct from the main API, so
        // it uses a separate path set map.
        if (op.isCallbackRequest) {
            pathSetMap = this.callbacksPathSetMap;
        } else {
            pathSetMap = this.pathSetMap;
        }

        while (pathSetMap.containsKey(pathId)) {
            Map<String, String> pathSetEntry = pathSetMap.get(pathId);
            if (pathSetEntry.get("path").equals(op.path)) {
                found = true;
                break;
            }
            pathId = basePathId + pathIdTiebreaker;
            pathIdTiebreaker++;
        }

        boolean hasPathParams = !op.pathParams.isEmpty();

        // String for matching for path using a regex
        // Don't prefix with '^' so that the templates can put the
        // basePath on the front.
        String regex = op.path;
        // String for formatting the path for a client to make a request
        String formatPath = op.path;

        for (CodegenParameter param : op.pathParams) {
            // Replace {baseName} with {paramName} for format string
            String paramSearch = "{" + param.baseName + "}";
            String paramReplace = "{" + param.paramName + "}";

            formatPath = formatPath.replace(paramSearch, paramReplace);
        }

        // Handle runtime callback parameters. Runtime callback parameters
        // are different from regular path parameters:
        // - They begin with a "{$" sequence, which allows us to identify them.
        // - They can contain multiple path segments, so we need to use a different
        //   regular expression.
        // - They may contain special characters such as "#", "." and "/" which aren't
        //   valid in Rust identifiers.
        // In the future, we may support parsing them directly
        if (op.isCallbackRequest) {
            formatPath = formatPath.substring(1); // Callback paths are absolute so strip initial '/'

            List<String> params = new ArrayList<String>();

            Matcher match = Pattern.compile("\\{\\$[^}{]*\\}").matcher(op.path);

            while (match.find()) {
                String param = match.group();

                // Convert to a rust variable name
                String rustParam = tidyUpRuntimeCallbackParam(param);
                params.add(rustParam);

                // Convert to a format arg
                String formatParam = "{" + rustParam + "}";

                formatPath = formatPath.replace(param, formatParam);

                // Convert to a regex
                String newParam = "(?P<" + rustParam + ">.*)";

                regex = regex.replace(param, newParam);

                hasPathParams = true;
            }

            op.vendorExtensions.put("x-callback-params", params);
        }

        // Save off the regular expression and path details in the relevant
        // "pathSetMap", which we'll add to the source document that will be
        // processed by the templates.
        if (!found) {
            Map<String, String> pathSetEntry = new HashMap<String, String>();
            pathSetEntry.put("path", op.path);
            pathSetEntry.put("PATH_ID", pathId);

            if (hasPathParams) {
                pathSetEntry.put("hasPathParams", "true");
            }

            // Don't prefix with '^' so that the templates can put the
            // basePath on the front.
            for (CodegenParameter param : op.pathParams) {
                // Replace {baseName} with (?P<baseName>[^/?#]*) for regex
                // TODO: Sanitize baseName to avoid using '-' (see clippy::invalid_regex)
                String paramSearch = "{" + param.baseName + "}";
                String paramReplace = "(?P<" + param.baseName + ">[^/?#]*)";

                regex = regex.replace(paramSearch, paramReplace);
            }

            pathSetEntry.put("pathRegEx", regex + "$");
            pathSetMap.put(pathId, pathSetEntry);
        }

        String underscoredOperationId = underscore(op.operationId);
        op.vendorExtensions.put("x-operation-id", underscoredOperationId);
        op.vendorExtensions.put("x-uppercase-operation-id", underscoredOperationId.toUpperCase(Locale.ROOT));
        String vendorExtensionPath = op.path.replace("{", ":").replace("}", "");
        op.vendorExtensions.put("x-path", vendorExtensionPath);
        op.vendorExtensions.put("x-path-id", pathId);
        op.vendorExtensions.put("x-has-path-params", hasPathParams);
        op.vendorExtensions.put("x-path-format-string", formatPath);

        String vendorExtensionHttpMethod = op.httpMethod.toUpperCase(Locale.ROOT);
        op.vendorExtensions.put("x-http-method", vendorExtensionHttpMethod);

        boolean isDelete = op.httpMethod.toUpperCase(Locale.ROOT).equals("DELETE");
        op.vendorExtensions.put("x-is-delete", isDelete);

        if (isDelete) {
            additionalProperties.put("apiHasDeleteMethods", true);
        }

        if (!op.vendorExtensions.containsKey("x-must-use-response")) {
            // If there's more than one response, than by default the user must explicitly handle them
            op.vendorExtensions.put("x-must-use-response", op.responses.size() > 1);
        }

        for (CodegenParameter param : op.allParams) {
            processParam(param, op);
        }

        // We keep track of the 'default' model type for this API. If there are
        // *any* XML responses, then we set the default to XML, otherwise we
        // let the default be JSON. It would be odd for an API to want to use
        // both XML and JSON on a single operation, and if we don't know
        // anything then JSON is a more modern (ergo reasonable) choice.
        boolean defaultsToXml = false;

        // Determine the types that this operation produces. `getProducesInfo`
        // simply lists all the types, and then we add the correct imports to
        // the generated library.
        List<String> produces = new ArrayList<String>(getProducesInfo(openAPI, operation));
        boolean producesXml = false;
        boolean producesPlainText = false;
        if (produces != null && !produces.isEmpty()) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            for (String mimeType : produces) {
                Map<String, String> mediaType = new HashMap<String, String>();

                if (isMimetypeXml(mimeType)) {
                    additionalProperties.put("usesXml", true);
                    defaultsToXml = true;
                    producesXml = true;
                } else if (isMimetypePlain(mimeType)) {
                    producesPlainText = true;
                }

                mediaType.put("mediaType", mimeType);
                c.add(mediaType);
            }
            op.produces = c;
            op.hasProduces = true;
        }

        for (CodegenParameter param : op.headerParams) {
            processParam(param, op);

            // Give header params a name in camel case. CodegenParameters don't have a nameInCamelCase property.
            param.vendorExtensions.put("x-type-name", toModelName(param.baseName));
        }

        // Set for deduplication of response IDs
        Set<String> responseIds = new HashSet();

        for (CodegenResponse rsp : op.responses) {

            // Get the original API response so we get process the schema
            // directly.
            ApiResponse original;
            if ("0".equals(rsp.code)) {
                original = operation.getResponses().get("default");
            } else {
                original = operation.getResponses().get(rsp.code);
            }
            original = ModelUtils.getReferencedApiResponse(openAPI, original);
            String[] words = rsp.message.split("[^A-Za-z ]");

            // Create a unique responseID for this response.
            String responseId;

            if (rsp.vendorExtensions.containsKey("x-response-id")) {
                // If it's been specified directly, use that.
                responseId = (String) rsp.vendorExtensions.get("x-response-id");
            } else if ((words.length != 0) && (words[0].trim().length() != 0)) {
                // If there's a description, build it from the description.
                responseId = camelize(words[0].replace(" ", "_"));
            } else {
                // Otherwise fall back to the http response code.
                responseId = "Status" + rsp.code;
            }

            // Deduplicate response IDs that would otherwise contain the same
            // text. We rely on the ID being unique, but since we form it from
            // the raw description field we can't require that the spec writer
            // provides unique descriptions.
            int idTieBreaker = 2;
            while (responseIds.contains(responseId)) {
                String trial = String.format(Locale.ROOT, "%s_%d", responseId, idTieBreaker);
                if (!responseIds.contains(trial)) {
                    responseId = trial;
                } else {
                    idTieBreaker++;
                }
            }

            responseIds.add(responseId);

            String underscoredResponseId = underscore(responseId).toUpperCase(Locale.ROOT);
            rsp.vendorExtensions.put("x-response-id", responseId);
            rsp.vendorExtensions.put("x-uppercase-response-id", underscoredResponseId.toUpperCase(Locale.ROOT));
            rsp.vendorExtensions.put("x-uppercase-operation-id", underscoredOperationId.toUpperCase(Locale.ROOT));
            if (rsp.dataType != null) {
                String uppercaseDataType = (rsp.dataType.replace("models::", "")).toUpperCase(Locale.ROOT);
                rsp.vendorExtensions.put("x-uppercase-data-type", uppercaseDataType);

                // Get the mimetype which is produced by this response. Note
                // that although in general responses produces a set of
                // different mimetypes currently we only support 1 per
                // response.
                String firstProduces = null;

                if (original.getContent() != null) {
                    firstProduces = original.getContent().keySet().stream().findFirst().orElse(null);
                }

                // The output mime type. This allows us to do sensible fallback
                // to JSON/XML rather than using only the default operation
                // mimetype.
                String outputMime;

                if (firstProduces == null) {
                    if (producesXml) {
                        outputMime = xmlMimeType;
                    } else if (producesPlainText) {
                        if (bytesType.equals(rsp.dataType)) {
                            outputMime = octetMimeType;
                        } else {
                            outputMime = plainTextMimeType;
                        }
                    } else {
                        outputMime = jsonMimeType;
                    }
                } else {
                    // If we know exactly what mimetype this response is
                    // going to produce, then use that. If we have not found
                    // anything, then we'll fall back to the 'producesXXX'
                    // definitions we worked out above for the operation as a
                    // whole.
                    if (isMimetypeXml(firstProduces)) {
                        producesXml = true;
                        producesPlainText = false;
                    } else if (isMimetypePlain(firstProduces)) {
                        producesXml = false;
                        producesPlainText = true;
                    } else {
                        producesXml = false;
                        producesPlainText = false;
                    }

                    outputMime = firstProduces;
                }

                rsp.vendorExtensions.put("x-mime-type", outputMime);

                // Write out the type of data we actually expect this response
                // to make.
                if (producesXml) {
                    rsp.vendorExtensions.put("x-produces-xml", true);
                } else if (producesPlainText) {
                    // Plain text means that there is not structured data in
                    // this response. So it'll either be a UTF-8 encoded string
                    // 'plainText' or some generic 'bytes'.
                    //
                    // Note that we don't yet distinguish between string/binary
                    // and string/bytes - that is we don't auto-detect whether
                    // base64 encoding should be done. They both look like
                    // 'producesBytes'.
                    if (bytesType.equals(rsp.dataType)) {
                        rsp.vendorExtensions.put("x-produces-bytes", true);
                    } else {
                        rsp.vendorExtensions.put("x-produces-plain-text", true);
                    }
                } else {
                    rsp.vendorExtensions.put("x-produces-json", true);
                    // If the data type is just "object", then ensure that the
                    // Rust data type is "serde_json::Value".  This allows us
                    // to define APIs that can return arbitrary JSON bodies.
                    if ("object".equals(rsp.dataType)) {
                        rsp.dataType = "serde_json::Value";
                    }
                }

                Schema response = (Schema) rsp.schema;
                // Check whether we're returning an object with a defined XML namespace.
                if (response != null && (!StringUtils.isEmpty(response.get$ref()))) {
                    Schema model = definitions.get(ModelUtils.getSimpleRef(response.get$ref()));
                    if ((model != null)) {
                        XML xml = model.getXml();
                        if ((xml != null) && (xml.getNamespace() != null)) {
                            rsp.vendorExtensions.put("x-has-namespace", "true");
                        }
                    }
                }
            }
            for (CodegenProperty header : rsp.headers) {
                if (uuidType.equals(header.dataType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }
                header.nameInPascalCase = toModelName(header.baseName);
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }
        }

        for (CodegenParameter header : op.headerParams) {
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        for (CodegenProperty header : op.responseHeaders) {
            if (uuidType.equals(header.dataType)) {
                additionalProperties.put("apiUsesUuid", true);
            }
            header.nameInPascalCase = toModelName(header.baseName);
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        return op;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();

        for (CodegenOperation op : operationList) {
            postProcessOperationWithModels(op, allModels);
        }

        operationList.sort((one, another) -> {
            int params_compare = ObjectUtils.compare(one.pathParams.size(), another.pathParams.size());
            if (params_compare == 0) {
                return ObjectUtils.compare(one.operationId, another.operationId);
            } else {
                return params_compare;
            }
        });

        return objs;
    }

    private void postProcessOperationWithModels(CodegenOperation op, List<ModelMap> allModels) {
        boolean consumesPlainText = false;
        boolean consumesXml = false;

        if (op.consumes != null) {
            for (Map<String, String> consume : op.consumes) {
                if (consume.get("mediaType") != null) {
                    String mediaType = consume.get("mediaType");

                    if (isMimetypeXml(mediaType)) {
                        additionalProperties.put("usesXml", true);
                        consumesXml = true;
                    } else if (isMimetypePlain(mediaType)) {
                        consumesPlainText = true;
                    } else if (isMimetypeWwwFormUrlEncoded(mediaType)) {
                        op.vendorExtensions.put("x-consumes-form", true);
                        additionalProperties.put("usesUrlEncodedForm", true);
                    } else if (isMimetypeMultipartFormData(mediaType)) {
                        op.vendorExtensions.put("x-consumes-multipart", true);
                        op.vendorExtensions.put("x-consumes-multipart-form", true);
                        additionalProperties.put("apiUsesMultipartFormData", true);
                        additionalProperties.put("apiUsesMultipart", true);
                    } else if (isMimetypeMultipartRelated(mediaType)) {
                        op.vendorExtensions.put("x-consumes-multipart", true);
                        op.vendorExtensions.put("x-consumes-multipart-related", true);
                        additionalProperties.put("apiUsesMultipartRelated", true);
                        additionalProperties.put("apiUsesMultipart", true);
                    }
                }
            }
        }

        if (op.bodyParams.size() > 0 || op.formParams.size() > 0) {
            op.vendorExtensions.put("x-has-request-body", true);
        }

        // The CLI generates a structopt structure for each operation. This can only have a single
        // use of a short option, which comes from the parameter name, so we need to police
        // against duplicates
        HashMap<Character, CodegenParameter> availableOptions = new HashMap();

        for (CodegenParameter p : op.allParams) {
            if (p.isBoolean && p.isPrimitiveType) {
                char shortOption = p.paramName.charAt(0);
                if (shortOption == 'a' || shortOption == 'o' || shortOption == 'f') {
                    // These are used by serverAddress, output, and force
                    p.vendorExtensions.put("x-provide-cli-short-opt", false);
                } else if (availableOptions.containsKey(shortOption)) {
                    availableOptions.get(shortOption).vendorExtensions.put("x-provide-cli-short-opt", false);
                    p.vendorExtensions.put("x-provide-cli-short-opt", false);
                } else {
                    availableOptions.put(shortOption, p);
                    p.vendorExtensions.put("x-provide-cli-short-opt", true);
                }
            }
        }

        String underscoredOperationId = underscore(op.operationId).toUpperCase(Locale.ROOT);

        if (op.bodyParam != null) {
            // Default to consuming json
            op.bodyParam.vendorExtensions.put("x-uppercase-operation-id", underscoredOperationId);
            if (consumesXml) {
                op.vendorExtensions.put("x-consumes-basic", true);
                op.bodyParam.vendorExtensions.put("x-consumes-xml", true);
            } else if (consumesPlainText) {
                op.vendorExtensions.put("x-consumes-basic", true);
                op.bodyParam.vendorExtensions.put("x-consumes-plain-text", true);
            } else {
                op.vendorExtensions.put("x-consumes-basic", true);
                op.bodyParam.vendorExtensions.put("x-consumes-json", true);
            }
        }

        for (CodegenParameter param : op.bodyParams) {
            processParam(param, op);

            param.vendorExtensions.put("x-uppercase-operation-id", underscoredOperationId);

            // Default to producing json if nothing else is specified
            if (consumesXml) {
                op.vendorExtensions.put("x-consumes-basic", true);
                param.vendorExtensions.put("x-consumes-xml", true);
            } else if (consumesPlainText) {
                op.vendorExtensions.put("x-consumes-basic", true);
                param.vendorExtensions.put("x-consumes-plain-text", true);
            } else {
                op.vendorExtensions.put("x-consumes-basic", true);
                param.vendorExtensions.put("x-consumes-json", true);
            }
        }

        for (CodegenParameter param : op.queryParams) {
            // If the MIME type is JSON, mark it.  We don't currently support any other MIME types.
            if (param.contentType != null && isMimetypeJson(param.contentType)) {
                param.vendorExtensions.put("x-consumes-json", true);
            }
        }

        for (CodegenParameter param : op.formParams) {
            processParam(param, op);
        }

        for (CodegenParameter header : op.headerParams) {
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        for (CodegenProperty header : op.responseHeaders) {
            if (uuidType.equals(header.dataType)) {
                additionalProperties.put("apiUsesUuid", true);
            }
            header.nameInPascalCase = toModelName(header.baseName);
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        if (op.authMethods != null) {
            boolean headerAuthMethods = false;

            for (CodegenSecurity s : op.authMethods) {
                if (s.isApiKey && s.isKeyInHeader) {
                    s.vendorExtensions.put("x-api-key-name", toModelName(s.keyParamName));
                    headerAuthMethods = true;
                }

                if (s.isBasicBasic || s.isBasicBearer || s.isOAuth) {
                    headerAuthMethods = true;
                }
            }

            if (headerAuthMethods) {
                op.vendorExtensions.put("x-has-header-auth-methods", "true");
            }
        }

        for (CodegenCallback callback : op.callbacks) {
            for (CodegenCallback.Url url : callback.urls) {
                for (CodegenOperation innerOp : url.requests) {
                    postProcessOperationWithModels(innerOp, allModels);
                }
            }
        }
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return dataType != null && dataType.equals(typeMapping.get("File"));
    }

    /**
     * Add operation to group
     *
     * @param tag          name of the tag
     * @param resourcePath path of the resource
     * @param operation    OAS Operation object
     * @param co           Codegen Operation object
     * @param operations   map of Codegen operations
     */
    @SuppressWarnings("static-method")
    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            co, Map<String, List<CodegenOperation>> operations) {
        // only generate operation for the first tag of the tags
        if (tag != null && co.tags.size() > 1) {
            String expectedTag = sanitizeTag(co.tags.get(0).getName());
            if (!tag.equals(expectedTag)) {
                LOGGER.info("generated skip additional tag `{}` with operationId={}", tag, co.operationId);
                return;
            }
        }
        super.addOperationToGroup(tag, resourcePath, operation, co, operations);
    }

    // This is a really terrible hack. We're working around the fact that the
    // base version of `fromRequestBody` checks to see whether the body is a
    // ref. If so, it unwraps the reference and replaces it with its inner
    // type. This causes problems in rust-server, as it means that we use inner
    // types in the API, rather than the correct outer type.
    //
    // Thus, we grab the inner schema beforehand, and then tinker afterwards to
    // restore things to sensible values.
    @Override
    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        Schema original_schema = ModelUtils.getSchemaFromRequestBody(body);
        CodegenParameter codegenParameter = super.fromRequestBody(body, imports, bodyParameterName);

        if (StringUtils.isNotBlank(original_schema.get$ref())) {
            // Undo the mess `super.fromRequestBody` made - re-wrap the inner
            // type.
            codegenParameter.dataType = getTypeDeclaration(original_schema);
            codegenParameter.isPrimitiveType = false;
            codegenParameter.isArray = false;
            codegenParameter.isString = false;
            codegenParameter.isByteArray = ModelUtils.isByteArraySchema(original_schema);


            // This is a model, so should only have an example if explicitly
            // defined.
            if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
                codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
            } else if (!codegenParameter.required) {
                //mandatory parameter use the example in the yaml. if no example, it is also null.
                codegenParameter.example = null;
            }
        }

        return codegenParameter;
    }

    @Override
    public String getTypeDeclaration(String name) {
        return "models::" + name;
    }

    private String modelFromSchema(Schema schema) {
        String ref = null;

        if (schema != null) {
            ref = schema.get$ref();
        }

        if (ref != null && ref.indexOf("#/components/schemas/") == 0) {
            ref = toModelName(ref.substring("#/components/schemas/".length()));
        } else {
            ref = null;
        }

        return ref;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        LOGGER.trace("Getting type declaration for schema");

        String type;

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            String innerType = getTypeDeclaration(inner);
            type = typeMapping.get("array") + "<" + innerType + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = getTypeDeclaration(inner);
            StringBuilder typeDeclaration = new StringBuilder(typeMapping.get("map")).append("<").append(typeMapping.get("string")).append(", ");
            typeDeclaration.append(innerType).append(">");
            type = typeDeclaration.toString();
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            try {
                type = modelFromSchema(p);

                if (type != null) {
                    type = "models::" + type;
                    LOGGER.debug("Returning " + type + " from ref");
                }
            } catch (Exception e) {
                type = null;
                LOGGER.error("Error obtaining the datatype from schema (model): " + p + ". Error was: " + e.getMessage(), e);
            }
        } else if (p instanceof FileSchema) {
            type = typeMapping.get("File").toString();
        } else {
            type = super.getTypeDeclaration(p);
        }

        // We are using extrinsic nullability, rather than intrinsic, so we need to dig into the inner
        // layer of the referenced schema.
        Schema rp = ModelUtils.getReferencedSchema(openAPI, p);

        if (rp.getNullable() == Boolean.TRUE) {
            type = "swagger::Nullable<" + type + ">";
        }

        LOGGER.debug("Returning " + type + " for type declaration");

        return type;
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema inner = ModelUtils.getSchemaItems(p);
            return instantiationTypes.get("array") + "<" + getSchemaType(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return instantiationTypes.get("map") + "<" + typeMapping.get("string") + ", " + getSchemaType(inner) + ">";
        } else {
            return null;
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        LOGGER.trace("Creating model from schema: {}", model);

        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel mdl = super.fromModel(name, model);

        LOGGER.debug("fromModel (base end): " + mdl);

        if (!StringUtils.isEmpty(model.get$ref())) {
            String ref = ModelUtils.getSimpleRef(model.get$ref());
            String dataType = toModelName(ref);
            mdl.dataType = dataType;
            mdl.isAlias = false;
            LOGGER.debug("Schema for: " + name + " is wrapper for: " + dataType);
        }


        if (ModelUtils.isArraySchema(model)) {
            Schema inner = ModelUtils.getSchemaItems(model);
            String xmlName = null;

            // Detect XML list where the inner item is defined directly.
            if ((inner != null) &&
                    (inner.getXml() != null)) {
                xmlName = inner.getXml().getName();
            }

            // Detect XML list where the inner item is a reference.
            if (model.getXml() != null && model.getXml().getWrapped() &&
                    inner != null &&
                    !StringUtils.isEmpty(inner.get$ref())) {
                Schema inner_schema = allDefinitions.get(
                        ModelUtils.getSimpleRef(inner.get$ref()));

                if (inner_schema.getXml() != null &&
                        inner_schema.getXml().getName() != null) {
                    xmlName = inner_schema.getXml().getName();
                }
            }

            // If this model's items require wrapping in xml, squirrel away the
            // xml name so we can insert it into the relevant model fields.
            if (xmlName != null) {
                mdl.vendorExtensions.put("x-item-xml-name", xmlName);
                modelXmlNames.put("models::" + mdl.classname, xmlName);
            }
        } else if ((mdl.anyOf.size() > 0) || (mdl.oneOf.size() > 0)) {
            mdl.dataType = getSchemaType(model);
        }

        if (mdl.xmlNamespace != null) {
            additionalProperties.put("usesXmlNamespaces", true);
        }

        Schema modelAdditionalProperties = ModelUtils.getAdditionalProperties(model);

        if (modelAdditionalProperties != null) {
            mdl.additionalPropertiesType = getTypeDeclaration(modelAdditionalProperties);
        }

        // Does this support partial ordering?
        boolean partialOrdSupport = true;

        if (mdl.dataType != null && mdl.dataType.equals("object")) {
            // Object isn't a sensible default. Instead, we set it to
            // 'null'. This ensures that we treat this model as a struct
            // with multiple parameters.
            mdl.dataType = null;
        } else if ("map".equals(mdl.dataType)) {
            if (!mdl.allVars.isEmpty() || mdl.additionalPropertiesType == null) {
                // We don't yet support `additionalProperties` that also have
                // properties. If we see variables, we ignore the
                // `additionalProperties` type ('map') and warn the user. This
                // will produce code that compiles, but won't feature the
                // `additionalProperties` - but that's likely more useful to
                // the user than the alternative.
                LOGGER.warn("Ignoring additionalProperties (see https://github.com/OpenAPITools/openapi-generator/issues/318) alongside defined properties");
                mdl.dataType = null;
            } else {
                mdl.dataType = "std::collections::HashMap<String, " + mdl.additionalPropertiesType + ">";
                partialOrdSupport = false;
            }
        } else if (mdl.dataType != null && mdl.isAlias) {
            // We need to hack about with single-parameter models to
            // get them recognised correctly.
            mdl.isAlias = false;
            mdl.dataType = typeMapping.get(mdl.dataType);
        }

        if (uuidType.equals(mdl.dataType)) {
            additionalProperties.put("apiUsesUuid", true);
        }

        for (CodegenProperty prop : mdl.vars) {
            if (uuidType.equals(prop.dataType)) {
                additionalProperties.put("apiUsesUuid", true);
            }

            String xmlName = modelXmlNames.get(prop.dataType);
            if (xmlName != null) {
                prop.vendorExtensions.put("x-item-xml-name", xmlName);
            }
        }

        // Do we support doing ToString/FromStr conversions for query parameters?
        boolean toStringSupport = true;
        boolean isString = "String".equals(mdl.dataType);

        if (isString) {
            toStringSupport = true;
        } else if (mdl.dataType != null
                && (mdl.dataType.startsWith("swagger::OneOf") || mdl.dataType.startsWith("swagger::AnyOf"))) {
            toStringSupport = false;
            partialOrdSupport = false;
        } else if (mdl.getAdditionalPropertiesType() != null) {
            toStringSupport = false;
        } else if (model instanceof ComposedSchema) {
            for (Schema schema : ModelUtils.getInterfaces((ComposedSchema) model)) {
                if (additionalProperties != null) {
                    toStringSupport = false;
                }
            }
        }

        mdl.vendorExtensions.put("x-upper-case-name", name.toUpperCase(Locale.ROOT));
        mdl.vendorExtensions.put("x-is-string", isString);
        mdl.vendorExtensions.put("x-to-string-support", toStringSupport);
        mdl.vendorExtensions.put("x-partial-ord", partialOrdSupport);

        LOGGER.trace("Created model: " + name + ": " + mdl + " from schema: " + model);

        return mdl;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> bundle) {
        generateYAMLSpecFile(bundle);

        addPathSetMapToBundle(pathSetMap, bundle);

        // If we have callbacks, add the callbacks module, otherwise remove it
        boolean hasCallbacks = haveCallbacks(bundle);
        bundle.put("hasCallbacks", hasCallbacks);
        SupportingFile[] callbackFiles = {
                new SupportingFile("client-callbacks.mustache", "src/client", "callbacks.rs"),
                new SupportingFile("server-callbacks.mustache", "src/server", "callbacks.rs"),
                new SupportingFile("example-client-server.mustache", "examples/client", "server.rs")
        };
        for (SupportingFile callbackFile : callbackFiles) {
            if (hasCallbacks) {
                supportingFiles.add(callbackFile);
            } else {
                supportingFiles.remove(callbackFile);
            }
        }

        if (hasCallbacks) {
            Map<String, Object> callbackData = new HashMap();
            addPathSetMapToBundle(callbacksPathSetMap, callbackData);
            bundle.put("callbacks", callbackData);
        }

        // Flag whether we have any OAuth scopes
        Map<String, SecurityScheme> securitySchemeMap = openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);
        boolean hasAuthScopes = false;
        if (authMethods != null && !authMethods.isEmpty()) {
            for (CodegenSecurity authMethod : authMethods) {
                if (authMethod.hasScopes != null && authMethod.hasScopes) {
                    hasAuthScopes = true;
                    break;
                }
            }
        }
        bundle.put("hasAuthScopes", hasAuthScopes);

        return super.postProcessSupportingFileData(bundle);
    }

    /**
     * Add a built path set map to the provided bundle
     *
     * @param pathSetMap A previously built path set map
     * @param bundle     Bundle for the supporting files to add the data to.
     */
    private static void addPathSetMapToBundle(Map<String, Map<String, String>> pathSetMap, Map<String, Object> bundle) {
        // We previously built a mapping from path to path ID and regular
        // expression - see fromOperation for details.  Sort it and add an
        // index, and then add it to the objects that we're about to pass to
        // the templates to process.
        List<Map.Entry<String, Map<String, String>>> pathSetEntryList = new ArrayList(pathSetMap.entrySet());
        Collections.sort(pathSetEntryList, new Comparator<Map.Entry<String, Map<String, String>>>() {
            @Override
            public int compare(Map.Entry<String, Map<String, String>> a, Map.Entry<String, Map<String, String>> b) {
                return a.getValue().get("path").compareTo(b.getValue().get("path"));
            }
        });
        List pathSet = new ArrayList<Map<String, String>>();
        int index = 0;
        for (Map.Entry<String, Map<String, String>> pathSetEntry : pathSetEntryList) {
            Map<String, String> pathSetEntryValue = pathSetEntry.getValue();
            pathSetEntryValue.put("index", Integer.toString(index));
            index++;
            pathSet.add(pathSetEntryValue);
        }
        bundle.put("pathSet", pathSet);
    }

    /**
     * Does the API being generated use callbacks?
     *
     * @param bundle Bundle data from DefaultGenerator which will be passed to the templates
     * @return true if any operation has a callback, false otherwise
     */
    private static boolean haveCallbacks(Map<String, Object> bundle) {
        ApiInfoMap apiInfo = (ApiInfoMap) bundle.get("apiInfo");
        for (OperationsMap api : apiInfo.getApis()) {
            List<CodegenOperation> ops = api.getOperations().getOperation();
            for (CodegenOperation op : ops) {
                if (!op.callbacks.isEmpty()) {
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    public String toDefaultValue(Schema p) {
        String defaultValue = null;
        if ((ModelUtils.isNullable(p)) && (p.getDefault() != null) && ("null".equalsIgnoreCase(p.getDefault().toString())))
            return "swagger::Nullable::Null";
        else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if ("false".equalsIgnoreCase(p.getDefault().toString()))
                    defaultValue = "false";
                else
                    defaultValue = "true";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                defaultValue = "\"" + String.valueOf(p.getDefault()) + "\".to_string()";
            }
        }
        if ((defaultValue != null) && (ModelUtils.isNullable(p)))
            defaultValue = "swagger::Nullable::Present(" + defaultValue + ")";
        return defaultValue;
    }

    @Override
    public String toOneOfName(List<String> names, Schema composedSchema) {
        Map<String, Object> exts = null;
        if (composedSchema != null) {
            exts = composedSchema.getExtensions();
        }
        if (exts != null && exts.containsKey("x-one-of-name")) {
            return (String) exts.get("x-one-of-name");
        }

        List<Schema> schemas = ModelUtils.getInterfaces(composedSchema);

        List<String> types = new ArrayList<>();
        for (Schema s : schemas) {
            types.add(getTypeDeclaration(s));
        }
        return "swagger::OneOf" + types.size() + "<" + String.join(",", types) + ">";
    }

    @Override
    public String toAnyOfName(List<String> names, Schema composedSchema) {
        List<Schema> schemas = ModelUtils.getInterfaces(composedSchema);

        List<String> types = new ArrayList<>();
        for (Schema s : schemas) {
            types.add(getTypeDeclaration(s));
        }
        return "swagger::AnyOf" + types.size() + "<" + String.join(",", types) + ">";
    }

    /**
     * Strip a swagger::Nullable wrapper on a datatype
     *
     * @deprecated Avoid using this - use a different mechanism instead.
     */
    private static String stripNullable(String type) {
        if (type.startsWith("swagger::Nullable<") && type.endsWith(">")) {
            return type.substring("swagger::Nullable<".length(), type.length() - 1);
        } else {
            return type;
        }
    }

    @Override
    public String toAllOfName(List<String> names, Schema composedSchema) {
        // Handle all of objects as freeform
        return null;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // TODO: We should avoid reverse engineering primitive type status from the data type
        if (!languageSpecificPrimitives.contains(stripNullable(property.dataType))) {
            // If we use a more qualified model name, then only camelize the actual type, not the qualifier.
            if (property.dataType.contains(":")) {
                int position = property.dataType.lastIndexOf(":");
                property.dataType = property.dataType.substring(0, position) + camelize(property.dataType.substring(position));
            } else {
                property.dataType = camelize(property.dataType);
            }
            property.isPrimitiveType = property.isContainer && languageSpecificPrimitives.contains(typeMapping.get(stripNullable(property.complexType)));
        } else {
            property.isPrimitiveType = true;
        }

        // Integer type fitting
        if (Objects.equals(property.baseType, "integer")) {

            BigInteger minimum = Optional.ofNullable(property.getMinimum()).map(BigInteger::new).orElse(null);
            BigInteger maximum = Optional.ofNullable(property.getMaximum()).map(BigInteger::new).orElse(null);

            boolean unsigned = canFitIntoUnsigned(minimum, property.getExclusiveMinimum());

            if (Strings.isNullOrEmpty(property.dataFormat)) {
                property.dataType = bestFittingIntegerType(minimum,
                        property.getExclusiveMinimum(),
                        maximum,
                        property.getExclusiveMaximum(),
                        true);
            } else {
                switch (property.dataFormat) {
                    // custom integer formats (legacy)
                    case "uint32":
                        property.dataType = "u32";
                        break;
                    case "uint64":
                        property.dataType = "u64";
                        break;
                    case "int32":
                        property.dataType = unsigned ? "u32" : "i32";
                        break;
                    case "int64":
                        property.dataType = unsigned ? "u64" : "i64";
                        break;
                    default:
                        LOGGER.warn("The integer format '{}' is not recognized and will be ignored.", property.dataFormat);
                        property.dataType = bestFittingIntegerType(minimum,
                                property.getExclusiveMinimum(),
                                maximum,
                                property.getExclusiveMaximum(),
                                true);
                }
            }
        }

        property.name = underscore(property.name);

        if (!property.required) {
            property.defaultValue = (property.defaultValue != null) ? "Some(" + property.defaultValue + ")" : "None";
        }

        // If a property has no type defined in the schema, it can take values of any type.
        // This clashes with Rust being statically typed. Hence, assume it's sent as a json
        // blob and return the json value to the user of the API and let the user determine
        // the type from the value. If the property has no type, at this point it will have
        // baseType "object" allowing us to identify such properties. Moreover, set to not
        // nullable, we can use the serde_json::Value::Null enum variant.
        if ("object".equals(property.baseType)) {
            property.dataType = "serde_json::Value";
            property.isNullable = false;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        return super.postProcessModelsEnum(objs);
    }

    private void processParam(CodegenParameter param, CodegenOperation op) {
        String example = null;

        // If a parameter uses UUIDs, we need to import the UUID package.
        if (uuidType.equals(param.dataType)) {
            additionalProperties.put("apiUsesUuid", true);
        }

        if (Boolean.TRUE.equals(param.isFreeFormObject)) {
            param.vendorExtensions.put("x-format-string", "{:?}");
            example = null;
        } else if (param.isArray && param.isString) {
            // This occurs if the parameter is a form property and is Vec<String>
            param.vendorExtensions.put("x-format-string", "{:?}");
            example = (param.example != null) ? "&vec![\"" + param.example + "\".to_string()]" : "&Vec::new()";
        } else if (param.isString) {
            param.vendorExtensions.put("x-format-string", "\\\"{}\\\"");
            example = "\"" + ((param.example != null) ? param.example : "") + "\".to_string()";
        } else if (param.isPrimitiveType) {
            if ((param.isByteArray) || (param.isBinary)) {
                // Binary primitive types don't implement `Display`.
                param.vendorExtensions.put("x-format-string", "{:?}");
                example = "swagger::ByteArray(Vec::from(\"" + ((param.example != null) ? param.example : "") + "\"))";
            } else {
                param.vendorExtensions.put("x-format-string", "{}");
                example = (param.example != null) ? param.example : "";
            }
        } else if (param.isArray) {
            param.vendorExtensions.put("x-format-string", "{:?}");
            if (param.items.isString) {
                // We iterate through the list of string and ensure they end up in the format vec!["example".to_string()]
                example = (param.example != null)
                    ? "&vec![" + Arrays.stream(param.example.replace("[", "").replace("]", "").split(","))
                        .map(item -> item + ".to_string()")
                        .collect(Collectors.joining(", ")) + "]"
                    : "&Vec::new()";
            } else {
                example = (param.example != null) ? param.example : "&Vec::new()";
            }
        } else {
            param.vendorExtensions.put("x-format-string", "{:?}");
            if (param.example != null) {
                example = "serde_json::from_str::<" + param.dataType + ">(r#\"" + param.example + "\"#).expect(\"Failed to parse JSON example\")";
            }
        }

        if (param.required) {
            if (example != null) {
                param.vendorExtensions.put("x-example", example);
            } else if (param.isArray) {
                // Use the empty list if we don't have an example
                param.vendorExtensions.put("x-example", "&Vec::new()");
            } else {
                // If we don't have an example that we can provide, we need to disable the client example, as it won't build.
                param.vendorExtensions.put("x-example", "???");
                op.vendorExtensions.put("x-no-client-example", Boolean.TRUE);
            }
        } else if ((param.dataFormat != null) && (("date-time".equals(param.dataFormat)) || ("date".equals(param.dataFormat)))) {
            param.vendorExtensions.put("x-format-string", "{:?}");
            param.vendorExtensions.put("x-example", "None");
        } else {
            // Not required, so override the format string and example
            param.vendorExtensions.put("x-format-string", "{:?}");
            String exampleString = (example != null) ? "Some(" + example + ")" : "None";
            param.vendorExtensions.put("x-example", exampleString);
        }
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }

        String commandPrefix = System.getenv("RUST_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(commandPrefix)) {
            commandPrefix = "rustfmt";
        }

        // only process files with .rs extension
        if ("rs".equals(FilenameUtils.getExtension(file.toString()))) {
            this.executePostProcessor(new String[]{commandPrefix, file.toString()});
        }
    }

    @Override
    protected void updateParameterForString(CodegenParameter codegenParameter, Schema parameterSchema) {
        /**
         * we have a custom version of this function to set isString to false for uuid
         */
        if (ModelUtils.isEmailSchema(parameterSchema)) {
            codegenParameter.isEmail = true;
        } else if (ModelUtils.isUUIDSchema(parameterSchema)) {
            codegenParameter.setIsString(false);
            codegenParameter.isUuid = true;
        } else if (ModelUtils.isByteArraySchema(parameterSchema)) {
            codegenParameter.setIsString(false);
            codegenParameter.isByteArray = true;
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isBinarySchema(parameterSchema)) {
            codegenParameter.isBinary = true;
            codegenParameter.isFile = true; // file = binary in OAS3
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isDateSchema(parameterSchema)) {
            codegenParameter.setIsString(false); // for backward compatibility with 2.x
            codegenParameter.isDate = true;
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isDateTimeSchema(parameterSchema)) {
            codegenParameter.setIsString(false); // for backward compatibility with 2.x
            codegenParameter.isDateTime = true;
            codegenParameter.isPrimitiveType = true;
        } else if (ModelUtils.isDecimalSchema(parameterSchema)) { // type: string, format: number
            codegenParameter.setIsString(false);
            codegenParameter.isDecimal = true;
            codegenParameter.isPrimitiveType = true;
        }
        if (Boolean.TRUE.equals(codegenParameter.isString)) {
            codegenParameter.isPrimitiveType = true;
        }
    }

    @Override
    protected void updatePropertyForAnyType(CodegenProperty property, Schema p) {
        /**
         * we have a custom version of this function to not set isNullable to true
         */
        // The 'null' value is allowed when the OAS schema is 'any type'.
        // See https://github.com/OAI/OpenAPI-Specification/issues/1389
        if (Boolean.FALSE.equals(p.getNullable())) {
            LOGGER.warn("Schema '{}' is any type, which includes the 'null' value. 'nullable' cannot be set to 'false'", p.getName());
        }
        if (languageSpecificPrimitives.contains(property.dataType)) {
            property.isPrimitiveType = true;
        }
        if (ModelUtils.isMapSchema(p)) {
            // an object or anyType composed schema that has additionalProperties set
            // some of our code assumes that any type schema with properties defined will be a map
            // even though it should allow in any type and have map constraints for properties
            updatePropertyForMap(property, p);
        }
    }

    @Override
    protected String getParameterDataType(Parameter parameter, Schema schema) {
        if (parameter.get$ref() != null) {
            String refName = ModelUtils.getSimpleRef(parameter.get$ref());
            return toModelName(refName);
        }
        return null;
    }
}
