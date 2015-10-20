package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.codegen.CliOption;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;

import org.apache.commons.lang.StringUtils;

public class CSharpClientCodegen extends DefaultCodegen implements CodegenConfig {
    protected String packageName = "IO.Swagger";
    protected String packageVersion = "1.0.0";
    protected String clientPackage = "IO.Swagger.Client";
    protected String sourceFolder = "src" + File.separator + "main" + File.separator + "csharp";

    public CSharpClientCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "csharp";
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");
        templateDir = "csharp";
        apiPackage = "IO.Swagger.Api";
        modelPackage = "IO.Swagger.Model";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using", "virtual", "void", "volatile", "while")
        );


        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "string",
                        "bool?",
                        "double?",
                        "int?",
                        "long?",
                        "float?",
                        "byte[]",
                        "List",
                        "Dictionary",
                        "DateTime?",
                        "String",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "Stream", // not really a primitive, we include it to avoid model import
                        "Object")
        );
        instantiationTypes.put("array", "List");
        instantiationTypes.put("map", "Dictionary");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "string");
        typeMapping.put("boolean", "bool?");
        typeMapping.put("integer", "int?");
        typeMapping.put("float", "float?");
        typeMapping.put("long", "long?");
        typeMapping.put("double", "double?");
        typeMapping.put("number", "double?");
        typeMapping.put("datetime", "DateTime?");
        typeMapping.put("date", "DateTime?");
        typeMapping.put("file", "Stream");
        typeMapping.put("array", "List");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Dictionary");
        typeMapping.put("object", "Object");

        cliOptions.clear();
        cliOptions.add(new CliOption("packageName", "C# package name (convention: Camel.Case), default: IO.Swagger"));
        cliOptions.add(new CliOption("packageVersion", "C# package version, default: 1.0.0"));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey("packageVersion")) {
            packageVersion = (String) additionalProperties.get("packageVersion");
        } else {
            additionalProperties.put("packageVersion", packageVersion);
        }

        if (additionalProperties.containsKey("packageName")) {
            packageName = (String) additionalProperties.get("packageName");
            apiPackage = packageName + ".Api";
            modelPackage = packageName + ".Model";
            clientPackage = packageName + ".Client";
        } else {
            additionalProperties.put("packageName", packageName);
        }

        additionalProperties.put("clientPackage", clientPackage);
        
        supportingFiles.add(new SupportingFile("Configuration.mustache",
                sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator), "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator), "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator), "ApiException.cs"));
        supportingFiles.add(new SupportingFile("Newtonsoft.Json.dll", "bin", "Newtonsoft.Json.dll"));
        supportingFiles.add(new SupportingFile("RestSharp.dll", "bin", "RestSharp.dll"));
        supportingFiles.add(new SupportingFile("compile.mustache", "", "compile.bat"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));

    }

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "csharp";
    }

    public String getHelp() {
        return "Generates a CSharp client library.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name 
        name = sanitizeName(name);

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize(lower) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
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
        // should be the same as the model name
        return toModelName(name);
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

            return getSwaggerType(p) + "<string, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType.toLowerCase())) {
            type = typeMapping.get(swaggerType.toLowerCase());
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
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(sanitizeName(operationId));
    }

}
