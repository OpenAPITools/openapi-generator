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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class HaskellServantCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(HaskellServantCodegen.class);

    // source folder where to write the files
    protected String sourceFolder = "src";
    protected String apiVersion = "0.0.1";
    private static final Pattern LEADING_UNDERSCORE = Pattern.compile("^_+");

    public static final String PROP_SERVE_STATIC = "serveStatic";
    public static final String PROP_SERVE_STATIC_DESC = "serve will serve files from the directory 'static'.";
    public static final Boolean PROP_SERVE_STATIC_DEFAULT = Boolean.TRUE;

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     */
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "haskell";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a Haskell server and client library.";
    }

    public HaskellServantCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        // override the mapping to keep the original mapping in Haskell
        specialCharReplacements.put("-", "Dash");
        specialCharReplacements.put(">", "GreaterThan");
        specialCharReplacements.put("<", "LessThan");

        // backslash and double quote need double the escapement for both Java and Haskell
        specialCharReplacements.remove("\\");
        specialCharReplacements.remove("\"");
        specialCharReplacements.put("\\\\", "Back_Slash");
        specialCharReplacements.put("\\\"", "Double_Quote");

        // set the output folder here
        outputFolder = "generated-code/haskell-servant";

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "haskell-servant";

        /*
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "API";

        /*
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "Types";

        // Haskell keywords and reserved function names, taken mostly from https://wiki.haskell.org/Keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // Keywords
                        "as", "case", "of",
                        "class", "data", "family",
                        "default", "deriving",
                        "do", "forall", "foreign", "hiding",
                        "if", "then", "else",
                        "import", "infix", "infixl", "infixr",
                        "instance", "let", "in",
                        "mdo", "module", "newtype",
                        "proc", "qualified", "rec",
                        "type", "where"
                )
        );

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("stack.mustache", "", "stack.yaml"));
        supportingFiles.add(new SupportingFile("Setup.mustache", "", "Setup.hs"));

        /*
         * Language Specific Primitives.  These types will not trigger imports by
         * the client generator
         */
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Bool",
                        "String",
                        "Int",
                        "Integer",
                        "Float",
                        "Char",
                        "Double",
                        "List",
                        "FilePath"
                )
        );

        typeMapping.clear();
        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "Text");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Int");
        typeMapping.put("char", "Char");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("DateTime", "UTCTime");
        typeMapping.put("Date", "Day");
        typeMapping.put("file", "FilePath");
        typeMapping.put("binary", "FilePath");
        typeMapping.put("number", "Double");
        typeMapping.put("BigDecimal", "Double");
        typeMapping.put("any", "Value");
        typeMapping.put("AnyType", "Value");
        typeMapping.put("UUID", "UUID");
        typeMapping.put("URI", "Text");
        typeMapping.put("ByteArray", "Text");
        typeMapping.put("object", "Value");

        importMapping.clear();
        importMapping.put("Map", "qualified Data.Map as Map");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(PROP_SERVE_STATIC, PROP_SERVE_STATIC_DESC).defaultValue(PROP_SERVE_STATIC_DEFAULT.toString()));
    }

    public void setBooleanProperty(String property, Boolean defaultValue) {
        if (additionalProperties.containsKey(property)) {
            additionalProperties.put(property, convertPropertyToBoolean(property));
        } else {
            additionalProperties.put(property, defaultValue);
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("HASKELL_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable HASKELL_POST_PROCESS_FILE not defined so the Haskell code may not be properly formatted. To define it, try 'export HASKELL_POST_PROCESS_FILE=\"$HOME/.local/bin/hfmt -w\"' (Linux/Mac)");
        }

        setBooleanProperty(PROP_SERVE_STATIC, PROP_SERVE_STATIC_DEFAULT);
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
        return "_" + name;
    }

    public String firstLetterToUpper(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toUpperCase(Locale.ROOT);
        } else {
            return word.substring(0, 1).toUpperCase(Locale.ROOT) + word.substring(1);
        }
    }

    public String firstLetterToLower(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toLowerCase(Locale.ROOT);
        } else {
            return word.substring(0, 1).toLowerCase(Locale.ROOT) + word.substring(1);
        }
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        // From the title, compute a reasonable name for the package and the API
        String title = openAPI.getInfo().getTitle();

        // Drop any API suffix
        if (title == null) {
            title = "OpenAPI";
        } else {
            title = title.trim();
            if (title.toUpperCase(Locale.ROOT).endsWith("API")) {
                title = title.substring(0, title.length() - 3);
            }
        }

        String[] words = title.split(" ");

        // The package name is made by appending the lowercased words of the title interspersed with dashes
        List<String> wordsLower = new ArrayList<>();
        for (String word : words) {
            wordsLower.add(word.toLowerCase(Locale.ROOT));
        }
        String cabalName = joinStrings("-", wordsLower);

        // The API name is made by appending the capitalized words of the title
        List<String> wordsCaps = new ArrayList<>();
        for (String word : words) {
            wordsCaps.add(firstLetterToUpper(word));
        }
        String apiName = joinStrings("", wordsCaps);

        // Set the filenames to write for the API
        supportingFiles.add(new SupportingFile("haskell-servant-codegen.mustache", "", cabalName + ".cabal"));
        supportingFiles.add(new SupportingFile("API.mustache", "lib/" + apiName, "API.hs"));
        supportingFiles.add(new SupportingFile("Types.mustache", "lib/" + apiName, "Types.hs"));


        additionalProperties.put("title", apiName);
        additionalProperties.put("titleLower", firstLetterToLower(apiName));
        additionalProperties.put("package", cabalName);

        // Due to the way servant resolves types, we need a high context stack limit
        additionalProperties.put("contextStackLimit", openAPI.getPaths().size() * 2 + 300);

        List<Map<String, Object>> replacements = new ArrayList<>();
        Object[] replacementChars = specialCharReplacements.keySet().toArray();
        for (Object replacementChar : replacementChars) {
            String c = (String) replacementChar;
            Map<String, Object> o = new HashMap<>();
            o.put("char", c);
            o.put("replacement", "'" + specialCharReplacements.get(c));
            replacements.add(o);
        }
        additionalProperties.put("specialCharReplacements", replacements);

        // See docstring for setGenerateToSchema for why we do this
        additionalProperties.put("generateToSchema", true);

        super.preprocessOpenAPI(openAPI);
    }

    /**
     * Internal method to set the generateToSchema parameter.
     *
     * Basically we're generating ToSchema instances (generically) for all schemas.
     * However, if any of the contained datatypes doesn't have the ToSchema instance,
     * we cannot generate it for its "ancestor" type.
     * This is the case with the "Data.Aeson.Value" type: it doesn't (and cannot) have
     * a Swagger-compatible ToSchema instance. So we have to detect its presence "downstream"
     * the current schema, and if we find it we just don't generate any ToSchema instance.
     * @param model
     */
    private void setGenerateToSchema(CodegenModel model) {
        for (CodegenProperty var : model.vars) {
            if (var.dataType.contentEquals("Value") || var.dataType.contains(" Value")) {
                additionalProperties.put("generateToSchema", false);
            }
            if (var.items != null) {
                if (var.items.dataType.contentEquals("Value") || var.dataType.contains(" Value")) {
                    additionalProperties.put("generateToSchema", false);
                }
            }
        }

        List<CodegenModel> children = model.getChildren();
        if (children != null) {
            for(CodegenModel child : children) {
                setGenerateToSchema(child);
            }
        }
    }


    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return "(Map.Map String " + getTypeDeclaration(inner) + ")";
        }
        return fixModelChars(super.getTypeDeclaration(p));
    }

    /**
     * Optional - OpenAPI type conversion.  This is used to map OpenAPI types in a `Schema` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        LOGGER.debug("debugging OpenAPI type: {}, {} => {}", p.getType(), p.getFormat(), schemaType);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            return type;
            //if (languageSpecificPrimitives.contains(type))
            //    return toModelName(type);
        } else if (typeMapping.containsValue(schemaType)) {
            // TODO what's this case for?
            type = schemaType + "_";
        } else {
            type = schemaType;
        }
        // it's a model
        return toModelName(type);
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            Schema additionalProperties2 = getAdditionalProperties(p);
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property {}\n\tIn Property: {}", additionalProperties2, p);
            }
            String inner = getSchemaType(additionalProperties2);
            return "(Map.Map Text " + inner + ")";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            // Return only the inner type; the wrapping with QueryList is done
            // somewhere else, where we have access to the collection format.
            return inner;
        } else {
            return null;
        }
    }


    // Intersperse a separator string between a list of strings, like String.join.
    private String joinStrings(String sep, List<String> ss) {
        StringBuilder sb = new StringBuilder();
        for (String s : ss) {
            if (sb.length() > 0) {
                sb.append(sep);
            }
            sb.append(s);
        }
        return sb.toString();
    }

    // Convert an HTTP path to a Servant route, including captured parameters.
    // For example, the path /api/jobs/info/{id}/last would become:
    //      "api" :> "jobs" :> "info" :> Capture "id" IdType :> "last"
    // IdType is provided by the capture params.
    private List<String> pathToServantRoute(String path, List<CodegenParameter> pathParams) {
        // Map the capture params by their names.
        HashMap<String, String> captureTypes = new HashMap<>();
        for (CodegenParameter param : pathParams) {
            captureTypes.put(param.baseName, param.dataType);
        }

        // Properly handle root-only routes (#3256)
        if (path.contentEquals("/")) {
            return new ArrayList<>();
        }

        // Cut off the leading slash, if it is present.
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // Convert the path into a list of servant route components.
        List<String> pathComponents = new ArrayList<>();
        for (String piece : path.split("/")) {
            if (piece.startsWith("{") && piece.endsWith("}")) {
                String name = piece.substring(1, piece.length() - 1);
                pathComponents.add("Capture \"" + name + "\" " + captureTypes.get(name));
            } else {
                pathComponents.add("\"" + piece + "\"");
            }
        }

        // Intersperse the servant route pieces with :> to construct the final API type
        return pathComponents;
    }

    // Extract the arguments that are passed in the route path parameters
    private List<String> pathToClientType(String path, List<CodegenParameter> pathParams) {
        // Map the capture params by their names.
        HashMap<String, String> captureTypes = new HashMap<>();
        for (CodegenParameter param : pathParams) {
            captureTypes.put(param.baseName, param.dataType);
        }

        // Cut off the leading slash, if it is present.
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // Convert the path into a list of servant route components.
        List<String> type = new ArrayList<>();
        for (String piece : path.split("/")) {
            if (piece.startsWith("{") && piece.endsWith("}")) {
                String name = piece.substring(1, piece.length() - 1);
                type.add(captureTypes.get(name));
            }
        }

        return type;
    }


    @Override
    public CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(resourcePath, httpMethod, operation, servers);

        List<String> path = pathToServantRoute(op.path, op.pathParams);
        List<String> type = pathToClientType(op.path, op.pathParams);

        // Query parameters appended to routes
        for (CodegenParameter param : op.queryParams) {
            String paramType = param.dataType;
            if (param.isArray) {
                if (StringUtils.isEmpty(param.collectionFormat)) {
                    param.collectionFormat = "csv";
                }
                paramType = makeQueryListType(paramType, param.collectionFormat);
            }
            path.add("QueryParam \"" + param.baseName + "\" " + paramType);
            type.add("Maybe " + param.dataType);
        }

        // Either body or form data parameters appended to route
        // As far as I know, you cannot have two ReqBody routes.
        // Is it possible to have body params AND have form params?
        String bodyType = null;
        if (op.getHasBodyParam()) {
            for (CodegenParameter param : op.bodyParams) {
                path.add("ReqBody '[JSON] " + param.dataType);
                bodyType = param.dataType;
            }
        } else if (op.getHasFormParams()) {
            // Use the FormX data type, where X is the conglomerate of all things being passed
            String formName = "Form" + camelize(op.operationId);
            bodyType = formName;
            path.add("ReqBody '[FormUrlEncoded] " + formName);
        }

        if (bodyType != null) {
            type.add(bodyType);
        }

        // Special headers appended to route
        for (CodegenParameter param : op.headerParams) {
            path.add("Header \"" + param.baseName + "\" " + param.dataType);

            String paramType = param.dataType;
            if (param.isArray) {
                if (StringUtils.isEmpty(param.collectionFormat)) {
                    param.collectionFormat = "csv";
                }
                paramType = makeQueryListType(paramType, param.collectionFormat);
            }
            type.add("Maybe " + paramType);
        }

        // store form parameter name in the vendor extensions
        for (CodegenParameter param : op.formParams) {
            param.vendorExtensions.put("x-form-param-name", camelize(param.baseName));
        }

        // Add the HTTP method and return type
        String returnType = op.returnType;
        if (returnType == null || returnType.equals("null")) {
            returnType = "NoContent";
        }
        if (returnType.indexOf(" ") >= 0) {
            returnType = "(" + returnType + ")";
        }
        path.add("Verb '" + op.httpMethod.toUpperCase(Locale.ROOT) + " 200 '[JSON] " + returnType);
        type.add("m " + returnType);

        op.vendorExtensions.put("x-route-type", joinStrings(" :> ", path));
        op.vendorExtensions.put("x-client-type", joinStrings(" -> ", type));
        op.vendorExtensions.put("x-form-name", "Form" + camelize(op.operationId));
        for (CodegenParameter param : op.formParams) {
            param.vendorExtensions.put("x-form-prefix", camelize(op.operationId, true));
        }
        return op;
    }

    private String makeQueryListType(String type, String collectionFormat) {
        type = type.substring(1, type.length() - 1);
        switch (collectionFormat) {
            case "csv":
                return "(QueryList 'CommaSeparated (" + type + "))";
            case "tsv":
                return "(QueryList 'TabSeparated (" + type + "))";
            case "space":
            case "ssv":
                return "(QueryList 'SpaceSeparated (" + type + "))";
            case "pipes":
                return "(QueryList 'PipeSeparated (" + type + "))";
            case "multi":
                return "(QueryList 'MultiParamArray (" + type + "))";
            default:
                throw new UnsupportedOperationException(collectionFormat + " (collection format) not supported");
        }
    }

    private String fixOperatorChars(String string) {
        StringBuilder sb = new StringBuilder();
        String name = string;
        //Check if it is a reserved word, in which case the underscore is added when property name is generated.
        if (string.startsWith("_")) {
            if (reservedWords.contains(string.substring(1))) {
                name = string.substring(1);
            } else if (reservedWordsMappings.containsValue(string)) {
                name = LEADING_UNDERSCORE.matcher(string).replaceFirst("");
            }
        }
        for (char c : name.toCharArray()) {
            String cString = String.valueOf(c);
            if (specialCharReplacements.containsKey(cString)) {
                sb.append("'");
                sb.append(specialCharReplacements.get(cString));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }

    // Remove characters from a string that do not belong in a model classname
    private String fixModelChars(String string) {
        return string.replace(".", "").replace("-", "");
    }

    // Override fromModel to create the appropriate model namings
    @Override
    public CodegenModel fromModel(String name, Schema mod) {
        CodegenModel model = super.fromModel(name, mod);

        setGenerateToSchema(model);

        // Clean up the class name to remove invalid characters
        model.classname = fixModelChars(model.classname);
        if (typeMapping.containsValue(model.classname)) {
            model.classname += "_";
        }

        // From the model name, compute the prefix for the fields.
        String prefix = camelize(model.classname, true);
        for (CodegenProperty prop : model.vars) {
            prop.name = toVarName(prefix + camelize(fixOperatorChars(prop.name)));
        }

        // Create newtypes for things with non-object types
        String dataOrNewtype = "data";
        if (!"object".equals(model.dataType) && typeMapping.containsKey(model.dataType)) {
            String newtype = typeMapping.get(model.dataType);
            // note; newtype is a single lowercase word in Haskell (not separated by hyphen)
            model.vendorExtensions.put("x-custom-newtype", newtype);
        }

        // Provide the prefix as a vendor extension, so that it can be used in the ToJSON and FromJSON instances.
        model.vendorExtensions.put("x-prefix", prefix);
        model.vendorExtensions.put("x-data", dataOrNewtype);

        return model;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("{-", "{_-").replace("-}", "-_}");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String haskellPostProcessFile = System.getenv("HASKELL_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(haskellPostProcessFile)) {
            return; // skip if HASKELL_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with hs extension
        if ("hs".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = haskellPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }
}
