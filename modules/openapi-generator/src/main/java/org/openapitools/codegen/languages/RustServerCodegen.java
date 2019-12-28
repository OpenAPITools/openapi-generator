/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
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
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.FileSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.XML;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.Map.Entry;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class RustServerCodegen extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(RustServerCodegen.class);

    private HashMap<String, String> modelXmlNames = new HashMap<String, String>();

    private static final String NO_FORMAT = "%%NO_FORMAT";

    protected String apiVersion = "1.0.0";
    protected String serverHost = "localhost";
    protected int serverPort = 8080;
    protected String projectName = "openapi-server";
    protected String apiPath = "rust-server";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String packageName;
    protected String packageVersion;
    protected String externCrateName;
    protected Map<String, Map<String, String>> pathSetMap = new HashMap<String, Map<String, String>>();

    private static final String uuidType = "uuid::Uuid";
    private static final String bytesType = "swagger::ByteArray";

    private static final String xmlMimeType = "application/xml";
    private static final String octetMimeType = "application/octet-stream";
    private static final String plainMimeType = "text/plain";
    private static final String jsonMimeType = "application/json";

    public RustServerCodegen() {
        super();

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

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        // From https://doc.rust-lang.org/grammar.html#keywords
                        "abstract", "alignof", "as", "become", "box", "break", "const",
                        "continue", "crate", "do", "else", "enum", "extern", "false",
                        "final", "fn", "for", "if", "impl", "in", "let", "loop", "macro",
                        "match", "mod", "move", "mut", "offsetof", "override", "priv",
                        "proc", "pub", "pure", "ref", "return", "Self", "self", "sizeof",
                        "static", "struct", "super", "trait", "true", "type", "typeof",
                        "unsafe", "unsized", "use", "virtual", "where", "while", "yield"
                )
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<String>(
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
        instantiationTypes.put("map", "Map");

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
        typeMapping.put("date", "chrono::DateTime<chrono::Utc>");
        typeMapping.put("DateTime", "chrono::DateTime<chrono::Utc>");
        typeMapping.put("password", "String");
        typeMapping.put("File", bytesType);
        typeMapping.put("file", bytesType);
        typeMapping.put("array", "Vec");
        typeMapping.put("map", "HashMap");

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
        supportingFiles.add(new SupportingFile("cargo-config", ".cargo", "config"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("lib.mustache", "src", "lib.rs"));
        supportingFiles.add(new SupportingFile("models.mustache", "src", "models.rs"));
        supportingFiles.add(new SupportingFile("server-mod.mustache", "src/server", "mod.rs"));
        supportingFiles.add(new SupportingFile("server-context.mustache", "src/server", "context.rs"));
        supportingFiles.add(new SupportingFile("client-mod.mustache", "src/client", "mod.rs"));
        supportingFiles.add(new SupportingFile("mimetypes.mustache", "src", "mimetypes.rs"));
        supportingFiles.add(new SupportingFile("example-server.mustache", "examples", "server.rs"));
        supportingFiles.add(new SupportingFile("example-client.mustache", "examples", "client.rs"));
        supportingFiles.add(new SupportingFile("example-server_lib.mustache", "examples/server_lib", "mod.rs"));
        supportingFiles.add(new SupportingFile("example-ca.pem", "examples", "ca.pem"));
        supportingFiles.add(new SupportingFile("example-server-chain.pem", "examples", "server-chain.pem"));
        supportingFiles.add(new SupportingFile("example-server-key.pem", "examples", "server-key.pem"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

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
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        additionalProperties.put("externCrateName", externCrateName);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;

        // Also set the extern crate name, which has any '-' replace with a '_'.
        this.externCrateName = packageName.replace('-', '_');
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
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
     * Configures a friendly name for the generator.  This will be used by the generator
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
        return "Generates a Rust client/server library (beta) using the swagger-codegen project.";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();
        List<String> versionComponents = new ArrayList<>(Arrays.asList(info.getVersion().split("[.]")));
        if (versionComponents.size() < 1) {
            versionComponents.add("1");
        }
        while (versionComponents.size() < 3) {
            versionComponents.add("0");
        }
        info.setVersion(StringUtils.join(versionComponents, "."));

        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        additionalProperties.put("serverHost", url.getHost());
        additionalProperties.put("serverPort", URLPathUtils.getPort(url, 80));
    }

    @Override
    public String toApiName(String name) {
        if (name.isEmpty()) {
            return "default";
        }
        return underscore(name);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name; // add an underscore to the name
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
    public String toModelName(String name) {
        // camelize the model name
        // phone_number => PhoneNumber
        String camelizedName = camelize(toModelFilename(name));

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            camelizedName = "Model" + camelizedName;
            LOGGER.warn(camelizedName + " (reserved word) cannot be used as model name. Renamed to " + camelizedName);
        }

        // model name starts with number
        else if (name.matches("^\\d.*")) {
            // e.g. 200Response => Model200Response (after camelize)
            camelizedName = "Model" + camelizedName;
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelizedName);
        }

        return camelizedName;

    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name (stolen from RubyClientCodegen)
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        String sanitizedName = super.sanitizeName(name);
        // for reserved word or word starting with number, append _
        if (isReservedWord(sanitizedName) || sanitizedName.matches("^\\d.*")) {
            sanitizedName = escapeReservedWord(sanitizedName);
        }

        return underscore(sanitizedName);
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(operationId);
    }

    @Override
    public String toModelFilename(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + "Enum";
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        String var = null;
        if (value.isEmpty()) {
            var = "EMPTY";
        }

        // for symbol, e.g. $, #
        else if (getSymbolName(value) != null) {
            var = getSymbolName(value).toUpperCase(Locale.ROOT);
        }

        // number
        else if ("Integer".equals(datatype) || "Long".equals(datatype) ||
                "Float".equals(datatype) || "Double".equals(datatype)) {
            String varName = "NUMBER_" + value;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            var = varName;
        }

        // string
        var = value.replaceAll("\\W+", "_").toUpperCase(Locale.ROOT);
        if (var.matches("\\d.*")) {
            var = "_" + var;
        } else {
            var = sanitizeName(var);
        }
        return var;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("Integer".equals(datatype) || "Long".equals(datatype) ||
                "Float".equals(datatype) || "Double".equals(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.go => pet_api.go
        return underscore(name);
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
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name) + "_api";
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    private boolean isMimetypeXml(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(xmlMimeType);
    }

    private boolean isMimetypePlainText(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith(plainMimeType);
    }

    private boolean isMimetypeHtmlText(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("text/html");
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

    private boolean isMimetypePlain(String mimetype) {
      return isMimetypePlainText(mimetype) || isMimetypeHtmlText(mimetype) || isMimetypeOctetStream(mimetype);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        Map<String, Schema> definitions = ModelUtils.getSchemas(this.openAPI);
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

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
        while (pathSetMap.containsKey(pathId)) {
            Map<String, String> pathSetEntry = pathSetMap.get(pathId);
            if (pathSetEntry.get("path").equals(op.path)) {
                found = true;
                break;
            }
            pathId = basePathId + pathIdTiebreaker;
            pathIdTiebreaker++;
        }

        // Save off the regular expression and path details in the
        // "pathSetMap", which we'll add to the source document that will be
        // processed by the templates.
        if (!found) {
            Map<String, String> pathSetEntry = new HashMap<String, String>();
            pathSetEntry.put("path", op.path);
            pathSetEntry.put("PATH_ID", pathId);
            if (!op.pathParams.isEmpty()) {
                pathSetEntry.put("hasPathParams", "true");
            }
            // Don't prefix with '^' so that the templates can put the
            // basePath on the front.
            pathSetEntry.put("pathRegEx", op.path.replace("{", "(?P<").replace("}", ">[^/?#]*)") + "$");
            pathSetMap.put(pathId, pathSetEntry);
        }

        op.vendorExtensions.put("operation_id", underscore(op.operationId));
        op.vendorExtensions.put("uppercase_operation_id", underscore(op.operationId).toUpperCase(Locale.ROOT));
        op.vendorExtensions.put("path", op.path.replace("{", ":").replace("}", ""));
        op.vendorExtensions.put("PATH_ID", pathId);
        op.vendorExtensions.put("hasPathParams", !op.pathParams.isEmpty());
        op.vendorExtensions.put("HttpMethod", op.httpMethod.toUpperCase(Locale.ROOT));
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
            param.vendorExtensions.put("typeName", toModelName(param.baseName));
        }

        // Set for deduplication of response IDs
        Set<String> responseIds = new HashSet();

        for (CodegenResponse rsp : op.responses) {

            // Get the original API response so we get process the schema
            // directly.
            ApiResponse original;
            if (rsp.code == "0") {
                original = operation.getResponses().get("default");
            } else {
                original = operation.getResponses().get(rsp.code);
            }
            String[] words = rsp.message.split("[^A-Za-z ]");

            // Create a unique responseID for this response.
            String responseId;

            if (rsp.vendorExtensions.containsKey("x-responseId")) {
                // If it's been specified directly, use that.
                responseId = (String) rsp.vendorExtensions.get("x-responseId");
            } else if (words.length != 0) {
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

            rsp.vendorExtensions.put("x-responseId", responseId);
            rsp.vendorExtensions.put("x-uppercaseResponseId", underscore(responseId).toUpperCase(Locale.ROOT));
            rsp.vendorExtensions.put("uppercase_operation_id", underscore(op.operationId).toUpperCase(Locale.ROOT));
            if (rsp.dataType != null) {
                rsp.vendorExtensions.put("uppercase_data_type", (rsp.dataType.replace("models::", "")).toUpperCase(Locale.ROOT));

                // Get the mimetype which is produced by this response. Note
                // that although in general responses produces a set of
                // different mimetypes currently we only support 1 per
                // response.
                String firstProduces = null;

                if (original.getContent() != null) {
                    for (String mimetype : original.getContent().keySet()) {
                        firstProduces = mimetype;
                        break;
                    }
                }

                // The output mime type. This allows us to do sensible fallback
                // to JSON/XML rather than using only the default operation
                // mimetype.
                String outputMime;

                if (firstProduces == null) {
                    if (producesXml) {
                        outputMime = xmlMimeType;
                    } else if (producesPlainText) {
                        if (rsp.dataType.equals(bytesType)) {
                            outputMime = octetMimeType;
                        } else {
                            outputMime = plainMimeType;
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

                rsp.vendorExtensions.put("mimeType", outputMime);

                // Write out the type of data we actually expect this response
                // to make.
                if (producesXml) {
                    rsp.vendorExtensions.put("producesXml", true);
                } else if (producesPlainText) {
                    // Plain text means that there is not structured data in
                    // this response. So it'll either be a UTF-8 encoded string
                    // 'plainText' or some generic 'bytes'.
                    //
                    // Note that we don't yet distinguish between string/binary
                    // and string/bytes - that is we don't auto-detect whether
                    // base64 encoding should be done. They both look like
                    // 'producesBytes'.
                    if (rsp.dataType.equals(bytesType)) {
                        rsp.vendorExtensions.put("producesBytes", true);
                    } else {
                        rsp.vendorExtensions.put("producesPlainText", true);
                    }
                } else {
                    rsp.vendorExtensions.put("producesJson", true);
                    // If the data type is just "object", then ensure that the
                    // Rust data type is "serde_json::Value".  This allows us
                    // to define APIs that can return arbitrary JSON bodies.
                    if (rsp.dataType.equals("object")) {
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
                            rsp.vendorExtensions.put("has_namespace", "true");
                        }
                    }
                }
            }
            for (CodegenProperty header : rsp.headers) {
                if (header.dataType.equals(uuidType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }
                header.nameInCamelCase = toModelName(header.baseName);
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }
        }

        for (CodegenParameter header : op.headerParams) {
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        for (CodegenProperty header : op.responseHeaders) {
            if (header.dataType.equals(uuidType)) {
                additionalProperties.put("apiUsesUuid", true);
            }
            header.nameInCamelCase = toModelName(header.baseName);
            header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
        }

        return op;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");

        for (CodegenOperation op : operationList) {
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
                            additionalProperties.put("usesUrlEncodedForm", true);
                        } else if (isMimetypeMultipartFormData(mediaType)) {
                            op.vendorExtensions.put("consumesMultipart", true);
                            additionalProperties.put("apiUsesMultipart", true);
                        }
                    }
                }
            }

            if (op.bodyParam != null) {
                // Default to consuming json
                op.bodyParam.vendorExtensions.put("uppercase_operation_id", underscore(op.operationId).toUpperCase(Locale.ROOT));
                if (consumesXml) {
                    op.bodyParam.vendorExtensions.put("consumesXml", true);
                } else if (consumesPlainText) {
                    op.bodyParam.vendorExtensions.put("consumesPlainText", true);
                } else {
                    op.bodyParam.vendorExtensions.put("consumesJson", true);
                }
            }

            for (CodegenParameter param : op.bodyParams) {
                processParam(param, op);

                param.vendorExtensions.put("uppercase_operation_id", underscore(op.operationId).toUpperCase(Locale.ROOT));

                // Default to producing json if nothing else is specified
                if (consumesXml) {
                    param.vendorExtensions.put("consumesXml", true);
                } else if (consumesPlainText) {
                    param.vendorExtensions.put("consumesPlainText", true);
                } else {
                    param.vendorExtensions.put("consumesJson", true);
                }
            }

            for (CodegenParameter param : op.formParams) {
                processParam(param, op);
            }

            for (CodegenParameter header : op.headerParams) {
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }

            for (CodegenProperty header : op.responseHeaders) {
                if (header.dataType.equals(uuidType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }
                header.nameInCamelCase = toModelName(header.baseName);
                header.nameInLowerCase = header.baseName.toLowerCase(Locale.ROOT);
            }

            if (op.authMethods != null) {
                boolean headerAuthMethods = false;

                for (CodegenSecurity s : op.authMethods) {
                    if (s.isApiKey && s.isKeyInHeader) {
                        s.vendorExtensions.put("x-apiKeyName", toModelName(s.keyParamName));
                        headerAuthMethods = true;
                    }

                    if (s.isBasicBasic || s.isBasicBearer || s.isOAuth) {
                        headerAuthMethods = true;
                    }
                }

                if (headerAuthMethods) {
                    op.vendorExtensions.put("hasHeaderAuthMethods", "true");
                }
            }
        }

        return objs;
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return dataType != null && dataType.equals(typeMapping.get("File").toString());
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
            codegenParameter.isListContainer = false;
            codegenParameter.isString = false;
            codegenParameter.isByteArray = ModelUtils.isByteArraySchema(original_schema);


            // This is a model, so should only have an example if explicitly
            // defined.
            if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
                codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
            } else {
                codegenParameter.example = null;
            }
        }

        return codegenParameter;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            String innerType = getTypeDeclaration(inner);
            return typeMapping.get("array") + "<" + innerType + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = getTypeDeclaration(inner);
            StringBuilder typeDeclaration = new StringBuilder(typeMapping.get("map")).append("<").append(typeMapping.get("string")).append(", ");
            typeDeclaration.append(innerType).append(">");
            return typeDeclaration.toString();
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            String datatype;
            try {
                datatype = p.get$ref();

                if (datatype.indexOf("#/components/schemas/") == 0) {
                    datatype = toModelName(datatype.substring("#/components/schemas/".length()));
                    datatype = "models::" + datatype;
                }
            } catch (Exception e) {
                LOGGER.warn("Error obtaining the datatype from schema (model):" + p + ". Datatype default to Object");
                datatype = "Object";
                LOGGER.error(e.getMessage(), e);
            }
            return datatype;
        } else if (p instanceof FileSchema) {
            return typeMapping.get("File").toString();
        }

        return super.getTypeDeclaration(p);
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
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
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        CodegenModel mdl = super.fromModel(name, model);
        mdl.vendorExtensions.put("upperCaseName", name.toUpperCase(Locale.ROOT));
        if (!StringUtils.isEmpty(model.get$ref())) {
            Schema schema = allDefinitions.get(ModelUtils.getSimpleRef(model.get$ref()));
            mdl.dataType = typeMapping.get(schema.getType());
        }
        if (ModelUtils.isArraySchema(model)) {
            ArraySchema am = (ArraySchema) model;
            String xmlName = null;

            // Detect XML list where the inner item is defined directly.
            if ((am.getItems() != null) &&
                    (am.getItems().getXml() != null)) {
                xmlName = am.getItems().getXml().getName();
            }

            // Detect XML list where the inner item is a reference.
            if (am.getXml() != null && am.getXml().getWrapped() &&
                    am.getItems() != null &&
                    !StringUtils.isEmpty(am.getItems().get$ref())) {
                Schema inner_schema = allDefinitions.get(
                    ModelUtils.getSimpleRef(am.getItems().get$ref()));

                if (inner_schema.getXml() != null &&
                        inner_schema.getXml().getName() != null) {
                    xmlName = inner_schema.getXml().getName();
                }
            }

            // If this model's items require wrapping in xml, squirrel away the
            // xml name so we can insert it into the relevant model fields.
            if (xmlName != null) {
                mdl.vendorExtensions.put("itemXmlName", xmlName);
                modelXmlNames.put("models::" + mdl.classname, xmlName);
            }

            mdl.arrayModelType = toModelName(mdl.arrayModelType);
        }

        if (mdl.xmlNamespace != null) {
            additionalProperties.put("usesXmlNamespaces", true);
        }

        Schema additionalProperties = ModelUtils.getAdditionalProperties(model);

        if (additionalProperties != null) {
            mdl.additionalPropertiesType = getSchemaType(additionalProperties);
        }

        return mdl;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> newObjs = super.postProcessAllModels(objs);

        //Index all CodegenModels by model name.
        HashMap<String, CodegenModel> allModels = new HashMap<String, CodegenModel>();
        for (Entry<String, Object> entry : objs.entrySet()) {
            String modelName = toModelName(entry.getKey());
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                allModels.put(modelName, cm);
            }
        }

        for (Entry<String, CodegenModel> entry : allModels.entrySet()) {
            String modelName = entry.getKey();
            CodegenModel model = entry.getValue();

            if (uuidType.equals(model.dataType)) {
                additionalProperties.put("apiUsesUuid", true);
            }

            for (CodegenProperty prop : model.vars) {
                if (prop.dataType.equals(uuidType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }

                String xmlName = modelXmlNames.get(prop.dataType);
                if (xmlName != null) {
                    prop.vendorExtensions.put("itemXmlName", xmlName);
                }

                if (prop.dataType.equals(uuidType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }
            }
        }

        return newObjs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateYAMLSpecFile(objs);

        // We previously built a mapping from path to path ID and regular
        // expression - see fromOperation for details.  Sort it and add an
        // index, and then add it to the objects that we're about to pass to
        // the templates to process.
        List<Map.Entry<String, Map<String, String>>> pathSetEntryList = new ArrayList(pathSetMap.entrySet());
        Collections.sort(pathSetEntryList, new Comparator<Map.Entry<String, Map<String, String>>>() {
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
        objs.put("pathSet", pathSet);

        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (p.getDefault().toString().equalsIgnoreCase("false"))
                    return "false";
                else
                    return "true";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + (String) p.getDefault() + "\".to_string()";
            }
        }

        return null;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (!languageSpecificPrimitives.contains(property.dataType)) {
            // If we use a more qualified model name, then only camelize the actual type, not the qualifier.
            if (property.dataType.contains(":")) {
                int position = property.dataType.lastIndexOf(":");
                property.dataType = property.dataType.substring(0, position) + camelize(property.dataType.substring(position));
            } else {
                property.dataType = camelize(property.dataType, false);
            }
            property.isPrimitiveType = property.isContainer && languageSpecificPrimitives.contains(typeMapping.get(property.complexType));
        } else {
            property.isPrimitiveType = true;
        }

        if ("integer".equals(property.baseType)) {
            // custom integer formats (legacy)
            if ("uint32".equals(property.dataFormat)) {
                property.dataType = "u32";
            } else if ("uint64".equals(property.dataFormat)) {
                property.dataType = "u64";

            } else {
                // match int type to schema constraints
                Long inclusiveMinimum = property.minimum != null ? Long.parseLong(property.minimum) : null;
                if (inclusiveMinimum != null && property.exclusiveMinimum) {
                    inclusiveMinimum++;
                }

                // a signed int is required unless a minimum greater than zero is set
                boolean unsigned = inclusiveMinimum != null && inclusiveMinimum >= 0;

                Long inclusiveMaximum = property.maximum != null ? Long.parseLong(property.maximum) : null;
                if (inclusiveMaximum != null && property.exclusiveMaximum) {
                    inclusiveMaximum--;
                }

                switch (property.dataFormat == null ? NO_FORMAT : property.dataFormat) {
                    // standard swagger formats
                    case "int32":
                        property.dataType = unsigned ? "u32" : "i32";
                        break;

                    case "int64":
                        property.dataType = unsigned ? "u64" : "i64";
                        break;

                    case NO_FORMAT:
                        property.dataType = matchingIntType(unsigned, inclusiveMinimum, inclusiveMaximum);
                        break;

                    default:
                        // unknown format
                        LOGGER.warn("The integer format '{}' is not recognized and will be ignored.", property.dataFormat);
                        property.dataType = matchingIntType(unsigned, inclusiveMinimum, inclusiveMaximum);
                }
            }
        }

        property.name = underscore(property.name);

        if (!property.required) {
            property.defaultValue = (property.defaultValue != null) ? "Some(" + property.defaultValue + ")" : "None";
        }
    }

    private long requiredBits(Long bound, boolean unsigned) {
        if (bound == null) return 0;

        if (unsigned) {
            if (bound < 0) {
                throw new RuntimeException("Unsigned bound is negative: " + bound);
            }
            return 65L - Long.numberOfLeadingZeros(bound >> 1);
        }

        return 65L - Long.numberOfLeadingZeros(
                // signed bounds go from (-n) to (n - 1), i.e. i8 goes from -128 to 127
                bound < 0 ? Math.abs(bound) - 1 : bound);
    }

    private String matchingIntType(boolean unsigned, Long inclusiveMin, Long inclusiveMax) {
        long requiredMinBits = requiredBits(inclusiveMin, unsigned);
        long requiredMaxBits = requiredBits(inclusiveMax, unsigned);
        long requiredBits = Math.max(requiredMinBits, requiredMaxBits);

        if (requiredMaxBits == 0 && requiredMinBits <= 16) {
            // rust 'size' types are arch-specific and thus somewhat loose
            // so they are used when no format or maximum are specified
            // and as long as minimum stays within plausible smallest ptr size (16 bits)
            // this way all rust types are obtainable without defining custom formats
            // this behavior (default int size) could also follow a generator flag
            return unsigned ? "usize" : "isize";

        } else if (requiredBits <= 8) {
            return unsigned ? "u8" : "i8";

        } else if (requiredBits <= 16) {
            return unsigned ? "u16" : "i16";

        } else if (requiredBits <= 32) {
            return unsigned ? "u32" : "i32";
        }
        return unsigned ? "u64" : "i64";
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            if (cm.dataType != null && cm.dataType.equals("object")) {
                // Object isn't a sensible default. Instead, we set it to
                // 'null'. This ensures that we treat this model as a struct
                // with multiple parameters.
                cm.dataType = null;
            } else if (cm.dataType != null && cm.dataType.equals("map")) {
                if (!cm.allVars.isEmpty() || cm.additionalPropertiesType == null) {
                    // We don't yet support `additionalProperties` that also have
                    // properties. If we see variables, we ignore the
                    // `additionalProperties` type ('map') and warn the user. This
                    // will produce code that compiles, but won't feature the
                    // `additionalProperties` - but that's likely more useful to
                    // the user than the alternative.
                    LOGGER.warn("Ignoring additionalProperties (see https://github.com/OpenAPITools/openapi-generator/issues/318) alongside defined properties");
                    cm.dataType = null;
                }
                else
                {
                    String type;

                    if (typeMapping.containsKey(cm.additionalPropertiesType)) {
                        type = typeMapping.get(cm.additionalPropertiesType);
                    } else {
                        type = toModelName(cm.additionalPropertiesType);
                    }

                    cm.dataType = "HashMap<String, " + type + ">";
                }
            } else if (cm.dataType != null) {
                // We need to hack about with single-parameter models to
                // get them recognised correctly.
                cm.isAlias = false;
                cm.dataType = typeMapping.get(cm.dataType);

                if (uuidType.equals(cm.dataType)) {
                    additionalProperties.put("apiUsesUuid", true);
                }
            }

            cm.vendorExtensions.put("isString", "String".equals(cm.dataType));
        }
        return super.postProcessModelsEnum(objs);
    }

    private void processParam(CodegenParameter param, CodegenOperation op) {
        String example = null;

        // If a parameter uses UUIDs, we need to import the UUID package.
        if (param.dataType.equals(uuidType)) {
            additionalProperties.put("apiUsesUuid", true);
        }

        if (param.isString) {
            param.vendorExtensions.put("formatString", "\\\"{}\\\"");
            example = "\"" + ((param.example != null) ? param.example : "") + "\".to_string()";
        } else if (param.isPrimitiveType) {
            if ((param.isByteArray) || (param.isBinary)) {
                // Binary primitive types don't implement `Display`.
                param.vendorExtensions.put("formatString", "{:?}");
                example = "swagger::ByteArray(Vec::from(\"" + ((param.example != null) ? param.example : "") + "\"))";
            } else {
                param.vendorExtensions.put("formatString", "{}");
                example = (param.example != null) ? param.example : "";
            }
        } else if (param.isListContainer) {
            param.vendorExtensions.put("formatString", "{:?}");
            example = (param.example != null) ? param.example : "&Vec::new()";
        } else {
            param.vendorExtensions.put("formatString", "{:?}");
            if (param.example != null) {
                example = "serde_json::from_str::<" + param.dataType + ">(\"" + param.example + "\").expect(\"Failed to parse JSON example\")";
            }
        }

        if (param.required) {
            if (example != null) {
                param.vendorExtensions.put("example", example);
            } else if (param.isListContainer) {
                // Use the empty list if we don't have an example
                param.vendorExtensions.put("example", "&Vec::new()");
            } else {
                // If we don't have an example that we can provide, we need to disable the client example, as it won't build.
                param.vendorExtensions.put("example", "???");
                op.vendorExtensions.put("noClientExample", Boolean.TRUE);
            }
        } else if ((param.dataFormat != null) && ((param.dataFormat.equals("date-time")) || (param.dataFormat.equals("date")))) {
            param.vendorExtensions.put("formatString", "{:?}");
            param.vendorExtensions.put("example", "None");
        } else {
            // Not required, so override the format string and example
            param.vendorExtensions.put("formatString", "{:?}");
            param.vendorExtensions.put("example", (example != null) ? "Some(" + example + ")" : "None");
        }
    }
}
