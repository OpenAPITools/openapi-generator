package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.ModelImpl;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.*;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;

import java.util.*;

public class HaskellServantCodegen extends DefaultCodegen implements CodegenConfig {

    // source folder where to write the files
    protected String sourceFolder = "src";
    protected String apiVersion = "0.0.1";

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see io.swagger.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -l flag.
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

        // override the mapping to keep the original mapping in Haskell
        specialCharReplacements.put("-", "Dash");
        specialCharReplacements.put(">", "GreaterThan");
        specialCharReplacements.put("<", "LessThan");

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
        languageSpecificPrimitives = new HashSet<String>(
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
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Integer");
        typeMapping.put("short", "Int");
        typeMapping.put("char", "Char");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Double");
        typeMapping.put("DateTime", "Integer");
        typeMapping.put("file", "FilePath");
        typeMapping.put("number", "Double");
        typeMapping.put("integer", "Int");
        typeMapping.put("any", "Value");

        importMapping.clear();
        importMapping.put("Map", "qualified Data.Map as Map");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reseved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return name + "_";
    }

    public String firstLetterToUpper(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toUpperCase();
        } else {
            return word.substring(0, 1).toUpperCase() + word.substring(1);
        }
    }

    public String firstLetterToLower(String word) {
        if (word.length() == 0) {
            return word;
        } else if (word.length() == 1) {
            return word.substring(0, 1).toLowerCase();
        } else {
            return word.substring(0, 1).toLowerCase() + word.substring(1);
        }
    }

    @Override
    public void preprocessSwagger(Swagger swagger) {
        // From the title, compute a reasonable name for the package and the API
        String title = swagger.getInfo().getTitle();

        // Drop any API suffix
        if(title == null) {
            title = "Swagger";
        } else {
            title = title.trim();
            if (title.toUpperCase().endsWith("API")) {
                title = title.substring(0, title.length() - 3);
            }
        }

        String[] words = title.split(" ");

        // The package name is made by appending the lowercased words of the title interspersed with dashes
        List<String> wordsLower = new ArrayList<String>();
        for (String word : words) {
            wordsLower.add(word.toLowerCase());
        }
        String cabalName = joinStrings("-", wordsLower);

        // The API name is made by appending the capitalized words of the title
        List<String> wordsCaps = new ArrayList<String>();
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
        additionalProperties.put("contextStackLimit", swagger.getPaths().size() * 2 + 300);

        List<Map<String, Object>> replacements = new ArrayList<>();
        Object[] replacementChars = specialCharReplacements.keySet().toArray();
        for(int i = 0; i < replacementChars.length; i++) {
            String c = (String) replacementChars[i];
            Map<String, Object> o = new HashMap<>();
            o.put("char", c);
            o.put("replacement", "'" + specialCharReplacements.get(c));
            o.put("hasMore", i != replacementChars.length - 1);
            replacements.add(o);
        }
        additionalProperties.put("specialCharReplacements", replacements);

        super.preprocessSwagger(swagger);
    }


    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[" + getTypeDeclaration(inner) + "]";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "Map.Map String " + getTypeDeclaration(inner);
        }
        return fixModelChars(super.getTypeDeclaration(p));
    }

    /**
     * Optional - swagger type conversion.  This is used to map swagger types in a `Property` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     * @see io.swagger.models.properties.Property
     */
    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return toModelName(type);
        } else if(swaggerType == "object") {
            type = "Value";
        } else if(typeMapping.containsValue(swaggerType)) {
            type = swaggerType + "_";
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toInstantiationType(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            Property additionalProperties2 = ap.getAdditionalProperties();
            String type = additionalProperties2.getType();
            if (null == type) {
                LOGGER.error("No Type defined for Additional Property " + additionalProperties2 + "\n" //
                        + "\tIn Property: " + p);
            }
            String inner = getSwaggerType(additionalProperties2);
            return "(Map.Map Text " + inner + ")";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
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
        HashMap<String, String> captureTypes = new HashMap<String, String>();
        for (CodegenParameter param : pathParams) {
            captureTypes.put(param.baseName, param.dataType);
        }

        // Cut off the leading slash, if it is present.
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // Convert the path into a list of servant route components.
        List<String> pathComponents = new ArrayList<String>();
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
        HashMap<String, String> captureTypes = new HashMap<String, String>();
        for (CodegenParameter param : pathParams) {
            captureTypes.put(param.baseName, param.dataType);
        }

        // Cut off the leading slash, if it is present.
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // Convert the path into a list of servant route components.
        List<String> type = new ArrayList<String>();
        for (String piece : path.split("/")) {
            if (piece.startsWith("{") && piece.endsWith("}")) {
                String name = piece.substring(1, piece.length() - 1);
                type.add(captureTypes.get(name));
            }
        }

        return type;
    }


    @Override
    public CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(resourcePath, httpMethod, operation, definitions, swagger);

        List<String> path = pathToServantRoute(op.path, op.pathParams);
        List<String> type = pathToClientType(op.path, op.pathParams);

        // Query parameters appended to routes
        for (CodegenParameter param : op.queryParams) {
            String paramType = param.dataType;
            if(param.isListContainer != null && param.isListContainer) {
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
        } else if(op.getHasFormParams()) {
            // Use the FormX data type, where X is the conglomerate of all things being passed
            String formName = "Form" + camelize(op.operationId);
            bodyType = formName;
            path.add("ReqBody '[FormUrlEncoded] " + formName);
        }
        if(bodyType != null) {
            type.add(bodyType);
        }

        // Special headers appended to route
        for (CodegenParameter param : op.headerParams) {
            path.add("Header \"" + param.baseName + "\" " + param.dataType);

            String paramType = param.dataType;
            if(param.isListContainer != null && param.isListContainer) {
                paramType = makeQueryListType(paramType, param.collectionFormat);
            }
            type.add("Maybe " + paramType);
        }

        // Add the HTTP method and return type
        String returnType = op.returnType;
        if (returnType == null || returnType.equals("null")) {
            returnType = "()";
        }
        if (returnType.indexOf(" ") >= 0) {
            returnType = "(" + returnType + ")";
        }
        path.add("Verb '" + op.httpMethod.toUpperCase() + " 200 '[JSON] " + returnType);
        type.add("m " + returnType);

        op.vendorExtensions.put("x-routeType", joinStrings(" :> ", path));
        op.vendorExtensions.put("x-clientType", joinStrings(" -> ", type));
        op.vendorExtensions.put("x-formName", "Form" + camelize(op.operationId));
        for(CodegenParameter param : op.formParams) {
            param.vendorExtensions.put("x-formPrefix", camelize(op.operationId, true));
        }
        return op;
    }

    private String makeQueryListType(String type, String collectionFormat) {
        type = type.substring(1, type.length() - 1);
        switch(collectionFormat) {
            case "csv": return "(QueryList 'CommaSeparated (" + type + "))";
            case "tsv": return "(QueryList 'TabSeparated (" + type + "))";
            case "ssv": return "(QueryList 'SpaceSeparated (" + type + "))";
            case "pipes": return "(QueryList 'PipeSeparated (" + type + "))";
            case "multi": return "(QueryList 'MultiParamArray (" + type + "))";
            default:
                throw new UnsupportedOperationException();
        }
    }

    private String fixOperatorChars(String string) {
        StringBuilder sb = new StringBuilder();
        for (char c : string.toCharArray()) {
            if (specialCharReplacements.containsKey(c)) {
                sb.append("'");
                sb.append(specialCharReplacements.get(c));
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
    public CodegenModel fromModel(String name, Model mod, Map<String, Model> allDefinitions) {
        CodegenModel model = super.fromModel(name, mod, allDefinitions);

        // Clean up the class name to remove invalid characters
        model.classname = fixModelChars(model.classname);
        if(typeMapping.containsValue(model.classname)) {
            model.classname += "_";
        }

        // From the model name, compute the prefix for the fields.
        String prefix = camelize(model.classname, true);
        for(CodegenProperty prop : model.vars) {
            prop.name = prefix + camelize(fixOperatorChars(prop.name));
        }

        // Create newtypes for things with non-object types
        String dataOrNewtype = "data";
        // check if it's a ModelImpl before casting 
        if (!(mod instanceof ModelImpl)) {
            return model;
        }

        String modelType = ((ModelImpl)  mod).getType();
        if(modelType != "object" && typeMapping.containsKey(modelType)) {
            String newtype = typeMapping.get(modelType);
            model.vendorExtensions.put("x-customNewtype", newtype);
        }

        // Provide the prefix as a vendor extension, so that it can be used in the ToJSON and FromJSON instances.
        model.vendorExtensions.put("x-prefix", prefix);
        model.vendorExtensions.put("x-data", dataOrNewtype);

        return model;
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        CodegenParameter p = super.fromParameter(param, imports);
        p.vendorExtensions.put("x-formParamName", camelize(p.baseName));
        p.dataType = fixModelChars(p.dataType);
        return p;
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

}
