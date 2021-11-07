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
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.dashize;

public class HaskellYesodServerCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "projectName";
    public static final String API_MODULE_NAME = "apiModuleName";

    private static final Pattern LEADING_UNDERSCORE = Pattern.compile("^_+");

    private final Logger LOGGER = LoggerFactory.getLogger(HaskellYesodServerCodegen.class);

    protected String projectName;
    protected String apiModuleName;

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "haskell-yesod";
    }

    public String getHelp() {
        return "Generates a haskell-yesod server.";
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getApiModuleName() {
        return apiModuleName;
    }

    public void setApiModuleName(String apiModuleName) {
        this.apiModuleName = apiModuleName;
    }

    public HaskellYesodServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.Callbacks
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
        );

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // override the mapping to keep the original mapping in Haskell
        specialCharReplacements.put("-", "Dash");
        specialCharReplacements.put(">", "GreaterThan");
        specialCharReplacements.put("<", "LessThan");

        // backslash and double quote need double the escapement for both Java and Haskell
        specialCharReplacements.remove("\\");
        specialCharReplacements.remove("\"");
        specialCharReplacements.put("\\\\", "Back_Slash");
        specialCharReplacements.put("\\\"", "Double_Quote");

        outputFolder = "generated-code" + File.separator + "haskell-yesod";
        apiTemplateFiles.put("api.mustache", ".hs");
        apiTestTemplateFiles.put("api_test.mustache", ".hs");
        embeddedTemplateDir = templateDir = "haskell-yesod";
        apiNameSuffix = "";

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

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "Bool",
                        "Int",
                        "Int64",
                        "Float",
                        "Double",
                        "Text",
                        "Day",
                        "UTCTime"
                )
        );

        typeMapping.clear();
        typeMapping.put("boolean", "Bool");     // type:boolean
        typeMapping.put("integer", "Int");      // type:integer+format:int32, type:integer
        typeMapping.put("long", "Int64");       // type:integer+format:int64
        typeMapping.put("number", "Double");    // type:number
        typeMapping.put("float", "Float");      // type:number+format:float
        typeMapping.put("double", "Double");    // type:number+format:double
        typeMapping.put("string", "Text");      // type:string
        typeMapping.put("date", "Day");         // type:string+format:date
        typeMapping.put("DateTime", "UTCTime"); // type:string+format:date-time
        typeMapping.put("decimal", "Text");     // type:string+format:number
        typeMapping.put("URI", "Text");         // type:string+format:uri
        typeMapping.put("UUID", "Text");        // type:string+format:uuid
        typeMapping.put("ByteArray", "Text");   // type:string+format:byte
        typeMapping.put("binary", "Text");      // type:string+format:binary
        typeMapping.put("file", "Text");        // type:string+format:binary(OAS3), type:file(OAS2)
        typeMapping.put("AnyType", "Value");    // type not specified

        // See getTypeDeclaration() for the followings.
        // typeMapping.put("array", "List");           // type:array (ArraySchema)
        // typeMapping.put("set", "List");             // type:array+uniqueItems:true (ArraySchema)
        // typeMapping.put("map", "Map.Map");          // type:object+additionalProperties:true/<object> (MapSchema)

        // type:object is defined as a separate data type, so the type mapping is not required.
        // typeMapping.put("object", "Value");         // type:object

        importMapping.clear();

        cliOptions.add(new CliOption(PROJECT_NAME,
                "name of the project (Default: generated from info.title or \"openapi-haskell-yesod-server\")"));
        cliOptions.add(new CliOption(API_MODULE_NAME,
                "name of the API module (Default: generated from info.title or \"API\")"));
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "src" + File.separator + "Handler";
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + "test" + File.separator + "Handler";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiName(name) + "Spec";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("HASKELL_POST_PROCESS_FILE"))) {
            LOGGER.info("Hint: Environment variable HASKELL_POST_PROCESS_FILE not defined so the Haskell code may not be properly formatted. To define it, try 'export HASKELL_POST_PROCESS_FILE=\"$HOME/.local/bin/hfmt -w\"' (Linux/Mac)");
        }

        if (additionalProperties.containsKey(PROJECT_NAME)) {
            this.setProjectName((String) additionalProperties.get(PROJECT_NAME));
        }
        if (additionalProperties.containsKey(API_MODULE_NAME)) {
            this.setApiModuleName((String) additionalProperties.get(API_MODULE_NAME));
        }
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

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            if (StringUtils.isBlank(projectName) && info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                projectName = dashize(sanitizeName(info.getTitle()));
            }
            if (StringUtils.isBlank(apiModuleName) && info.getTitle() != null) {
                // when apiModuleName is not specified, generate it from info.title
                apiModuleName = camelize(sanitizeName(info.getTitle()));
            }
        }

        // default values
        if (StringUtils.isBlank(projectName)) {
            projectName = "openapi-haskell-yesod-server";
        }
        if (StringUtils.isBlank(apiModuleName)) {
            apiModuleName = "API";
        }

        additionalProperties.put(PROJECT_NAME, projectName);
        additionalProperties.put(API_MODULE_NAME, apiModuleName);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("app/DevelMain.mustache", "app", "DevelMain.hs"));
        supportingFiles.add(new SupportingFile("app/devel.mustache", "app", "devel.hs"));
        supportingFiles.add(new SupportingFile("app/main.hs", "app", "main.hs"));
        supportingFiles.add(new SupportingFile("config/keter.mustache", "config", "keter.yml"));
        supportingFiles.add(new SupportingFile("config/routes.mustache", "config", "routes.yesodroutes"));
        supportingFiles.add(new SupportingFile("config/settings.yml", "config", "settings.yml"));
        supportingFiles.add(new SupportingFile("config/test-settings.yml", "config", "test-settings.yml"));
        supportingFiles.add(new SupportingFile("dir-locals.el", "", ".dir-locals.el"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.yaml"));
        supportingFiles.add(new SupportingFile("src/API/Types.mustache", "src" + File.separator + apiModuleName, "Types.hs"));
        supportingFiles.add(new SupportingFile("src/Application.mustache", "src", "Application.hs"));
        supportingFiles.add(new SupportingFile("src/Error.hs", "src", "Error.hs"));
        supportingFiles.add(new SupportingFile("src/Foundation.hs", "src", "Foundation.hs"));
        supportingFiles.add(new SupportingFile("src/Import/NoFoundation.mustache", "src" + File.separator + "Import", "NoFoundation.hs"));
        supportingFiles.add(new SupportingFile("src/Import.hs", "src", "Import.hs"));
        supportingFiles.add(new SupportingFile("src/Settings/StaticFiles.hs", "src" + File.separator + "Settings", "StaticFiles.hs"));
        supportingFiles.add(new SupportingFile("src/Settings.hs", "src", "Settings.hs"));
        supportingFiles.add(new SupportingFile("stack.yaml", "", "stack.yaml"));
        supportingFiles.add(new SupportingFile("static/gitkeep", "static", ".gitkeep"));
        supportingFiles.add(new SupportingFile("test/Spec.hs", "test", "Spec.hs"));
        supportingFiles.add(new SupportingFile("test/TestImport.hs", "test", "TestImport.hs"));

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

    private List<String> pathToComponents(String path, List<CodegenParameter> pathParams) {
        // Map the capture params by their names.
        HashMap<String, String> captureTypes = new HashMap<>();
        for (CodegenParameter param : pathParams) {
            captureTypes.put(param.baseName, param.dataType);
        }

        // Cut off the leading slash, if it is present.
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // Convert the path into a list of yesod path components.
        List<String> components = new ArrayList<>();
        for (String piece : path.split("/")) {
            if (piece.startsWith("{") && piece.endsWith("}")) {
                String name = piece.substring(1, piece.length() - 1);
                components.add("#" + captureTypes.get(name));
            } else {
                components.add(piece);
            }
        }

        return components;
    }

    private String pathToYesodPath(String path, List<CodegenParameter> pathParams) {
        return "/" + String.join("/", pathToComponents(path, pathParams));
    }

    private String pathToYesodResource(String path, List<CodegenParameter> pathParams) {
        String resource = "";
        for (String component : pathToComponents(path, pathParams)) {
            if (component.startsWith("#")) {
                resource += "By" + camelize(component.substring(1));
            } else {
                resource += camelize(component);
            }
        }
        if (resource.isEmpty()) {
            resource = camelize(apiModuleName) + "Home";
        }
        resource += "R";
        return resource;
    }

    @Override
    public CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(resourcePath, httpMethod, operation, servers);

        String path = pathToYesodPath(op.path, op.pathParams);
        String resource = pathToYesodResource(op.path, op.pathParams);

        List<Map<String, Object>> routes = (List<Map<String, Object>>) additionalProperties.get("routes");
        if (routes == null) {
            routes = new ArrayList<>();
            additionalProperties.put("routes", routes);
        }

        // https://www.yesodweb.com/book/routing-and-handlers#routing-and-handlers_overlap_checking
        if (hasOverlappedPath(path, routes)) {
            path = "!" + path;
        }

        Boolean found = false;
        for (Map<String, Object> route : routes) {
            if (path.equals(route.get("path"))) {
                List<String> methods = (List<String>) route.get("methods");
                methods.add(op.httpMethod);
                found = true;
                break;
            }
        }

        if (!found) {
            Map<String, Object> route = new HashMap<>();
            route.put("path", path);
            route.put("resource", resource);
            List<String> methods = new ArrayList<>();
            methods.add(op.httpMethod);
            route.put("methods", methods);
            routes.add(route);
        }

        // values used in api.mustache/api_test.mustache
        String handler = httpMethod.toLowerCase(Locale.ROOT) + resource;
        String paramIndent = StringUtils.repeat(" ", handler.length());
        op.vendorExtensions.put("x-handler", handler);
        op.vendorExtensions.put("x-param-indent", paramIndent);
        op.vendorExtensions.put("x-resource", resource);
        op.vendorExtensions.put("x-is-get-or-post", op.httpMethod.equals("GET") || op.httpMethod.equals("POST"));
        for (CodegenParameter param : op.pathParams) {
            param.vendorExtensions.put("x-handler", handler);
            param.vendorExtensions.put("x-param-indent", paramIndent);
            param.vendorExtensions.put("x-test-value", getParameterTestValue(param));
        }

        return op;
    }

    public Boolean hasOverlappedPath(String path, List<Map<String, Object>> routes) {
        for (Map<String, Object> route : routes) {
            String processedPath = (String) route.get("path");
            if (processedPath.startsWith("!")) {
                continue;
            }
            if (isOverlappedPath(path, processedPath)) {
                return true;
            }
        }
        return false;
    }

    public Boolean isOverlappedPath(String pathA, String pathB) {
        if (pathA.equals(pathB)) {
            return false;
        }

        String[] componentsA = pathA.split("/");
        String[] componentsB = pathB.split("/");
        if (componentsA.length != componentsB.length) {
            return false;
        }

        for (int i = 0; i < componentsA.length; i++) {
            if (componentsA[i].equals(componentsB[i])) {
                continue;
            } else if (componentsA[i].startsWith("#") || componentsB[i].startsWith("#")) {
                continue;
            } else {
                return false;
            }
        }
        return true;
    }

    private String getParameterTestValue(CodegenParameter codegenParameter) {
        if (Boolean.TRUE.equals(codegenParameter.isBoolean)) {
            return codegenParameter.example; // "true";
        } else if (Boolean.TRUE.equals(codegenParameter.isLong)) {
            return codegenParameter.example; // "789";
        } else if (Boolean.TRUE.equals(codegenParameter.isInteger)) {
            return codegenParameter.example; // "56";
        } else if (Boolean.TRUE.equals(codegenParameter.isFloat)) {
            return codegenParameter.example; // "3.4";
        } else if (Boolean.TRUE.equals(codegenParameter.isDouble)) {
            return codegenParameter.example; // "1.2";
        } else if (Boolean.TRUE.equals(codegenParameter.isNumber)) {
            return codegenParameter.example; // "8.14";
        } else if (Boolean.TRUE.equals(codegenParameter.isBinary)) {
            return "\"" + codegenParameter.example + "\""; // "BINARY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isByteArray)) {
            return "\"" + codegenParameter.example + "\""; // "BYTE_ARRAY_DATA_HERE";
        } else if (Boolean.TRUE.equals(codegenParameter.isFile)) {
            return "\"" + codegenParameter.example + "\""; // "/path/to/file.txt";
        } else if (Boolean.TRUE.equals(codegenParameter.isDate)) {
            return "\"" + codegenParameter.example + "\""; // "2013-10-20";
        } else if (Boolean.TRUE.equals(codegenParameter.isDateTime)) {
            return "\"" + codegenParameter.example + "\""; // "2013-10-20T19:20:30+01:00";
        } else if (Boolean.TRUE.equals(codegenParameter.isUuid)) {
            return "\"" + codegenParameter.example + "\""; // "38400000-8cf0-11bd-b23e-10b96e4ef00d";
        } else if (Boolean.TRUE.equals(codegenParameter.isUri)) {
            return "\"" + codegenParameter.example + "\""; // "https://openapi-generator.tech";
        } else if (Boolean.TRUE.equals(codegenParameter.isString)) {
            return "\"" + codegenParameter.example + "\""; // codegenParameter.paramName + "_example";
        } else if (Boolean.TRUE.equals(codegenParameter.isFreeFormObject)) {
            return "\"" + codegenParameter.example + "\""; // "Object";
        } else {
            return "unknown";
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

        // setGenerateToSchema(model);

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
