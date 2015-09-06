package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;
import java.util.HashMap;

public class DartClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected boolean browserClient = true;
    protected String pubName = "swagger";
    protected String pubVersion = "1.0.0";
    protected String pubDescription = "Swagger API client";
    protected String sourceFolder = "";

    public DartClientCodegen() {
        super();
        outputFolder = "generated-code/dart";
        modelTemplateFiles.put("model.mustache", ".dart");
        apiTemplateFiles.put("api.mustache", ".dart");
        templateDir = "dart";
        apiPackage = "lib.api";
        modelPackage = "lib.model";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "abstract", "as", "assert", "async", "async*", "await",
                        "break", "case", "catch", "class", "const", "continue",
                        "default", "deferred", "do", "dynamic", "else", "enum",
                        "export", "external", "extends", "factory", "false", "final",
                        "finally", "for", "get", "if", "implements", "import", "in",
                        "is", "library", "new", "null", "operator", "part", "rethrow",
                        "return", "set", "static", "super", "switch", "sync*", "this",
                        "throw", "true", "try", "typedef", "var", "void", "while",
                        "with", "yield", "yield*" )
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "bool",
                        "num",
                        "int",
                        "float")
        );
        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Map");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Array", "List");
        typeMapping.put("array", "List");
        typeMapping.put("List", "List");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "String");
        typeMapping.put("int", "int");
        typeMapping.put("float", "num");
        typeMapping.put("long", "int");
        typeMapping.put("short", "int");
        typeMapping.put("char", "String");
        typeMapping.put("double", "num");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "int");
        typeMapping.put("Date", "DateTime");
        typeMapping.put("date", "DateTime");
        typeMapping.put("File", "MultipartFile");

        cliOptions.add(new CliOption("browserClient", "Is the client browser based"));
        cliOptions.add(new CliOption("pubName", "Name in generated pubspec"));
        cliOptions.add(new CliOption("pubVersion", "Version in generated pubspec"));
        cliOptions.add(new CliOption("sourceFolder", "source folder for generated code"));
    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "dart";
    }

    public String getHelp() {
        return "Generates a Dart client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("browserClient")) {
            this.setBrowserClient(Boolean.parseBoolean((String) additionalProperties.get("browserClient")));
            additionalProperties.put("browserClient", browserClient);
        } else {
            //not set, use to be passed to template
            additionalProperties.put("browserClient", browserClient);
        }

        if (additionalProperties.containsKey("pubName")) {
            this.setPubName((String) additionalProperties.get("pubName"));
        } else {
            //not set, use to be passed to template
            additionalProperties.put("pubName", pubName);
        }

        if (additionalProperties.containsKey("pubVersion")) {
            this.setPubVersion((String) additionalProperties.get("pubVersion"));
        } else {
            //not set, use to be passed to template
            additionalProperties.put("pubVersion", pubVersion);
        }

        if (additionalProperties.containsKey("pubDescription")) {
            this.setPubDescription((String) additionalProperties.get("pubDescription"));
        } else {
            //not set, use to be passed to template
            additionalProperties.put("pubDescription", pubDescription);
        }

        if (additionalProperties.containsKey("sourceFolder")) {
            this.setSourceFolder((String) additionalProperties.get("sourceFolder"));
        }

        final String libFolder = sourceFolder + File.separator + "lib";
        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("api_client.mustache", libFolder, "api_client.dart"));
        supportingFiles.add(new SupportingFile("apiException.mustache", libFolder, "api_exception.dart"));
        supportingFiles.add(new SupportingFile("apilib.mustache", libFolder, "api.dart"));

        final String authFolder = sourceFolder + File.separator + "lib" + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/authentication.mustache", authFolder, "authentication.dart"));
        supportingFiles.add(new SupportingFile("auth/http_basic_auth.mustache", authFolder, "http_basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
    }


    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override
    public String toApiFilename(String name) {
        return underscore(toApiName(name));
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return "{}";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return "[]";
        }

        return super.toDefaultValue(p);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(operationId, true);
    }

    public void setBrowserClient(boolean browserClient) {
        this.browserClient = browserClient;
    }

    public void setPubName(String pubName) {
        this.pubName = pubName;
    }

    public void setPubVersion(String pubVersion) {
        this.pubVersion = pubVersion;
    }

    public void setPubDescription(String pubDescription) {
        this.pubDescription = pubDescription;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }
}
