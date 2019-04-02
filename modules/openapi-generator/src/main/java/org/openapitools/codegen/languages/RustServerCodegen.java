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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.FileSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.XML;
import io.swagger.v3.oas.models.parameters.Parameter;
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
        typeMapping.put("UUID", "uuid::Uuid");
        typeMapping.put("byte", "u8");
        typeMapping.put("ByteArray", "swagger::ByteArray");
        typeMapping.put("binary", "swagger::ByteArray");
        typeMapping.put("boolean", "bool");
        typeMapping.put("date", "chrono::DateTime<chrono::Utc>");
        typeMapping.put("DateTime", "chrono::DateTime<chrono::Utc>");
        typeMapping.put("password", "String");
        typeMapping.put("File", "swagger::ByteArray");
        typeMapping.put("file", "swagger::ByteArray");
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
        supportingFiles.add(new SupportingFile("example-server_server.mustache", "examples/server_lib", "server.rs"));
        supportingFiles.add(new SupportingFile("example-ca.pem", "examples", "ca.pem"));
        supportingFiles.add(new SupportingFile("example-server-chain.pem", "examples", "server-chain.pem"));
        supportingFiles.add(new SupportingFile("example-server-key.pem", "examples", "server-key.pem"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();

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

        URL url = URLPathUtils.getServerURL(openAPI);
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

    boolean isMimetypeXml(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("application/xml");
    }

    boolean isMimetypePlainText(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("text/plain");
    }

    boolean isMimetypeHtmlText(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("text/html");
    }

    boolean isMimetypeWwwFormUrlEncoded(String mimetype) {
        return mimetype.toLowerCase(Locale.ROOT).startsWith("application/x-www-form-urlencoded");
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
        op.vendorExtensions.put("HttpMethod", Character.toUpperCase(op.httpMethod.charAt(0)) + op.httpMethod.substring(1).toLowerCase(Locale.ROOT));
        for (CodegenParameter param : op.allParams) {
            processParam(param, op);
        }

        List<String> consumes = new ArrayList<String>();

        boolean consumesPlainText = false;
        boolean consumesXml = false;
        // if "consumes" is defined (per operation or using global definition)
        if (consumes != null && !consumes.isEmpty()) {
            consumes.addAll(getConsumesInfo(this.openAPI, operation));
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            for (String mimeType : consumes) {
                Map<String, String> mediaType = new HashMap<String, String>();

                if (isMimetypeXml(mimeType)) {
                    additionalProperties.put("usesXml", true);
                    consumesXml = true;
                } else if (isMimetypePlainText(mimeType)) {
                    consumesPlainText = true;
                } else if (isMimetypeHtmlText(mimeType)) {
                    consumesPlainText = true;
                } else if (isMimetypeWwwFormUrlEncoded(mimeType)) {
                    additionalProperties.put("usesUrlEncodedForm", true);
                }

                mediaType.put("mediaType", mimeType);
                c.add(mediaType);
            }
            op.consumes = c;
            op.hasConsumes = true;
        }


        List<String> produces = new ArrayList<String>(getProducesInfo(this.openAPI, operation));

        boolean producesXml = false;
        boolean producesPlainText = false;
        if (produces != null && !produces.isEmpty()) {
            List<Map<String, String>> c = new ArrayList<Map<String, String>>();
            for (String mimeType : produces) {
                Map<String, String> mediaType = new HashMap<String, String>();

                if (isMimetypeXml(mimeType)) {
                    additionalProperties.put("usesXml", true);
                    producesXml = true;
                } else if (isMimetypePlainText(mimeType)) {
                    producesPlainText = true;
                } else if (isMimetypeHtmlText(mimeType)) {
                    producesPlainText = true;
                }

                mediaType.put("mediaType", mimeType);
                c.add(mediaType);
            }
            op.produces = c;
            op.hasProduces = true;
        }

        for (CodegenParameter param : op.headerParams) {
            // If a header uses UUIDs, we need to import the UUID package.
            if (param.dataType.equals("uuid::Uuid")) {
                additionalProperties.put("apiUsesUuid", true);
            }
            processParam(param, op);

            // Give header params a name in camel case. CodegenParameters don't have a nameInCamelCase property.
            param.vendorExtensions.put("typeName", toModelName(param.baseName));
        }

        for (CodegenResponse rsp : op.responses) {
            String[] words = rsp.message.split("[^A-Za-z ]");
            String responseId;
            if (rsp.vendorExtensions.containsKey("x-responseId")) {
                responseId = (String) rsp.vendorExtensions.get("x-responseId");
            } else if (words.length != 0) {
                responseId = camelize(words[0].replace(" ", "_"));
            } else {
                responseId = "Status" + rsp.code;
            }
            rsp.vendorExtensions.put("x-responseId", responseId);
            rsp.vendorExtensions.put("x-uppercaseResponseId", underscore(responseId).toUpperCase(Locale.ROOT));
            rsp.vendorExtensions.put("uppercase_operation_id", underscore(op.operationId).toUpperCase(Locale.ROOT));
            if (rsp.dataType != null) {
                rsp.vendorExtensions.put("uppercase_data_type", (rsp.dataType.replace("models::", "")).toUpperCase(Locale.ROOT));

                // Default to producing json if nothing else is specified
                if (producesXml) {
                    rsp.vendorExtensions.put("producesXml", true);
                } else if (producesPlainText) {
                    rsp.vendorExtensions.put("producesPlainText", true);
                } else {
                    rsp.vendorExtensions.put("producesJson", true);
                    // If the data type is just "object", then ensure that the Rust data type
                    // is "serde_json::Value".  This allows us to define APIs that
                    // can return arbitrary JSON bodies.
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
                if (header.dataType.equals("uuid::Uuid")) {
                    additionalProperties.put("apiUsesUuid", true);
                }
                header.nameInCamelCase = toModelName(header.baseName);
            }
        }
        for (CodegenProperty header : op.responseHeaders) {
            if (header.dataType.equals("uuid::Uuid")) {
                additionalProperties.put("apiUsesUuid", true);
            }
            header.nameInCamelCase = toModelName(header.baseName);
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
                        } else if (isMimetypePlainText(mediaType) || isMimetypeHtmlText(mediaType)) {
                            consumesPlainText = true;
                        } else if (isMimetypeWwwFormUrlEncoded(mediaType)) {
                            additionalProperties.put("usesUrlEncodedForm", true);
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

            for (CodegenProperty header : op.responseHeaders) {
                if (header.dataType.equals("uuid::Uuid")) {
                    additionalProperties.put("apiUsesUuid", true);
                }
                header.nameInCamelCase = toModelName(header.baseName);
            }

            if (op.authMethods != null) {
                for (CodegenSecurity s : op.authMethods) {
                    if (s.isApiKey && s.isKeyInHeader) {
                        s.vendorExtensions.put("x-apiKeyName", toModelName(s.keyParamName));
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return dataType != null && dataType.equals(typeMapping.get("File").toString());
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            String innerType = getTypeDeclaration(inner);
            StringBuilder typeDeclaration = new StringBuilder(typeMapping.get("array")).append("<");
            typeDeclaration.append(innerType).append(">");
            return typeDeclaration.toString();
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

    private boolean isNonPrimitive(CodegenParameter parameter) {
        return !parameter.isString && !parameter.isNumeric && !parameter.isByteArray &&
            !parameter.isBinary && !parameter.isFile && !parameter.isBoolean &&
            !parameter.isDate && !parameter.isDateTime && !parameter.isUuid &&
            !parameter.isListContainer && !parameter.isMapContainer &&
            !languageSpecificPrimitives.contains(parameter.dataType);
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter parameter = super.fromParameter(param, imports);
        if (isNonPrimitive(parameter)) {
            String name = "models::" + getTypeDeclaration(parameter.dataType);
            parameter.dataType = name;
        }

        return parameter;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        // If this parameter is not a primitive type, prefix it with "models::"
        // to ensure it's namespaced correctly in the Rust code.
        if (isNonPrimitive(parameter)) {
            String name = "models::" + getTypeDeclaration(parameter.dataType);
            parameter.dataType = name;
        }
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
            if ((am.getItems() != null) &&
                    (am.getItems().getXml() != null)) {

                // If this model's items require wrapping in xml, squirrel
                // away the xml name so we can insert it into the relevant model fields.
                String xmlName = am.getItems().getXml().getName();
                if (xmlName != null) {
                    mdl.vendorExtensions.put("itemXmlName", xmlName);
                    modelXmlNames.put("models::" + mdl.classname, xmlName);
                }
            }
            mdl.arrayModelType = toModelName(mdl.arrayModelType);
        }

        if (mdl.xmlNamespace != null) {
            additionalProperties.put("usesXmlNamespaces", true);
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

            for (CodegenProperty prop : model.vars) {
                String xmlName = modelXmlNames.get(prop.dataType);
                if (xmlName != null) {
                    prop.vendorExtensions.put("itemXmlName", xmlName);
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

    static long requiredBits(Long bound, boolean unsigned) {
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

    static String matchingIntType(boolean unsigned, Long inclusiveMin, Long inclusiveMax) {
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
            } else if (cm.dataType != null) {
                if (cm.dataType.equals("map")) {
                    // We don't yet support `additionalProperties`. We ignore
                    // the `additionalProperties` type ('map') and warn the
                    // user. This will produce code that compiles, but won't
                    // feature the `additionalProperties`.
                    cm.dataType = null;
                    LOGGER.warn("Ignoring unsupported additionalProperties (see https://github.com/OpenAPITools/openapi-generator/issues/318)");
                } else {
                    // We need to hack about with single-parameter models to
                    // get them recognised correctly.
                    cm.isAlias = false;
                    cm.dataType = typeMapping.get(cm.dataType);
                }
            }
        }
        return super.postProcessModelsEnum(objs);
    }

    private boolean paramHasXmlNamespace(CodegenParameter param, Map<String, Schema> definitions) {
        Object refName = param.vendorExtensions.get("refName");

        if ((refName != null) && (refName instanceof String)) {
            String name = (String) refName;
            Schema model = definitions.get(ModelUtils.getSimpleRef(name));

            if (model != null) {
                XML xml = model.getXml();
                if ((xml != null) && (xml.getNamespace() != null)) {
                    return true;
                }
            }
        }
        return false;
    }

    private void processParam(CodegenParameter param, CodegenOperation op) {
        String example = null;

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
