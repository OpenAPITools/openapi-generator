package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class GoClientCodegen extends DefaultCodegen implements CodegenConfig {
    static Logger LOGGER = LoggerFactory.getLogger(GoClientCodegen.class);

    protected String packageName = "swagger";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "go";
    }

    public String getHelp() {
        return "Generates a Go client library (beta).";
    }

    public GoClientCodegen() {
        super();
        outputFolder = "generated-code/go";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        templateDir = "go";

        setReservedWordsLowerCase(
            Arrays.asList(
                "break", "default", "func", "interface", "select",
                "case", "defer", "go", "map", "struct",
                "chan", "else", "goto", "package", "switch",
                "const", "fallthrough", "if", "range", "type",
                "continue", "for", "import", "return", "var", "error", "ApiResponse")
                // Added "error" as it's used so frequently that it may as well be a keyword
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                    "map",
                    "array")
                );

        languageSpecificPrimitives = new HashSet<String>(
            Arrays.asList(
                "string",
                "bool",
                "uint",
                "uint32",
                "uint64",
                "int",
                "int32",
                "int64",
                "float32",
                "float64",
                "complex64",
                "complex128",
                "rune",
                "byte")
            );

        instantiationTypes.clear();
        /*instantiationTypes.put("array", "GoArray");
        instantiationTypes.put("map", "GoMap");*/

        typeMapping.clear();
        typeMapping.put("integer", "int32");
        typeMapping.put("long", "int64");
        typeMapping.put("number", "float32");
        typeMapping.put("float", "float32");
        typeMapping.put("double", "float64");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "string");
        typeMapping.put("date", "time.Time");
        typeMapping.put("DateTime", "time.Time");
        typeMapping.put("password", "string");
        typeMapping.put("File", "*os.File");
        typeMapping.put("file", "*os.File");
        // map binary to string as a workaround
        // the correct solution is to use []byte
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");

        importMapping = new HashMap<String, String>();
        importMapping.put("time.Time", "time");
        importMapping.put("*os.File", "os");
        importMapping.put("os", "io/ioutil");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Go package name (convention: lowercase).")
                .defaultValue("swagger"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "Go package version.")
                .defaultValue("1.0.0"));
    }

    @Override
    public void processOpts() {
        //super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }
        else {
            setPackageName("swagger");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }
        else {
            setPackageVersion("1.0.0");
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("configuration.mustache", "", "configuration.go"));
        supportingFiles.add(new SupportingFile("api_client.mustache", "", "api_client.go"));
        supportingFiles.add(new SupportingFile("api_response.mustache", "", "api_response.go"));
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
    }

    @Override
    public String escapeReservedWord(String name)
    {
        // Can't start with an underscore, as our fields need to start with an
        // UppercaseLetter so that Go treats them as public/visible.

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.

        // FIXME: This should also really be a customizable option
        return camelize(name) + '_';
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator;
    }

    public String modelFileFolder() {
        return outputFolder + File.separator;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_");

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // camelize (lower first character) the variable name
        // pet_id => PetId
        name = camelize(name);

        // for reserved word or word starting with number, append _
        if(isReservedWord(name) || name.matches("^\\d.*"))
            name = escapeReservedWord(name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        // params should be lowerCamelCase. E.g. "person Person", instead of
        // "Person Person".
        //
        // REVISIT: Actually, for idiomatic go, the param name should
        // really should just be a letter, e.g. "p Person"), but we'll get
        // around to that some other time... Maybe.
        return camelize(toVarName(name), true);
    }

    @Override
    public String toModelName(String name) {
        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(toModelFilename(name));
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
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.go => pet_api.go
        return underscore(name) + "_api";
    }



    /**
     * Overrides postProcessParameter to add a vendor extension "x-exportParamName".
     * This is useful when paramName starts with a lowercase letter, but we need that
     * param to be exportable (starts with an Uppercase letter).
     *
     * @param parameter CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter){

        // Give the base class a chance to process
        super.postProcessParameter(parameter);

        char firstChar = parameter.paramName.charAt(0);

        if (Character.isUpperCase(firstChar)) {
            // First char is already uppercase, just use paramName.
            parameter.vendorExtensions.put("x-exportParamName", parameter.paramName);

        }

        // It's a lowercase first char, let's convert it to uppercase
        StringBuilder sb = new StringBuilder(parameter.paramName);
        sb.setCharAt(0, Character.toUpperCase(firstChar));
        parameter.vendorExtensions.put("x-exportParamName", sb.toString());
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
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if(p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return "[]" + getTypeDeclaration(inner);
        }
        else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "[string]" + getTypeDeclaration(inner);
        }
        //return super.getTypeDeclaration(p);

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String swaggerType = getSwaggerType(p);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }

        if(typeMapping.containsValue(swaggerType)) {
            return swaggerType;
        }

        if(languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }

        return toModelName(swaggerType);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if(typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if(languageSpecificPrimitives.contains(type))
                return (type);
        }
        else
            type = swaggerType;
        return type;
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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion (e.g. PUT => Put)
            operation.httpMethod = camelize(operation.httpMethod.toLowerCase());
        }

        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        if (imports == null)
            return objs;

        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(apiPackage()))
                iterator.remove();
        }
        // if the return type is not primitive, import encoding/json
        for (CodegenOperation operation : operations) {
            if(operation.returnBaseType != null && needToImport(operation.returnBaseType)) {
                Map<String, String> customImport = new HashMap<String, String>();
                customImport.put("import", "encoding/json");
                imports.add(customImport);
                break; //just need to import once
            }
        }

        // recursivly add import for mapping one type to multipe imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = imports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                Map<String, String> newImportMap= new HashMap<String, String>();
                newImportMap.put("import", importMapping.get(_import));
                listIterator.add(newImportMap);
            }
        }

        return objs;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // remove model imports to avoid error
        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        final String prefix = modelPackage();
        Iterator<Map<String, String>> iterator = imports.iterator();
        while (iterator.hasNext()) {
            String _import = iterator.next().get("import");
            if (_import.startsWith(prefix))
                iterator.remove();
        }

        // recursivly add import for mapping one type to multipe imports
        List<Map<String, String>> recursiveImports = (List<Map<String, String>>) objs.get("imports");
        if (recursiveImports == null)
            return objs;

        ListIterator<Map<String, String>> listIterator = imports.listIterator();
        while (listIterator.hasNext()) {
            String _import = listIterator.next().get("import");
            // if the import package happens to be found in the importMapping (key)
            // add the corresponding import package to the list
            if (importMapping.containsKey(_import)) {
                Map<String, String> newImportMap= new HashMap<String, String>();
                newImportMap.put("import", importMapping.get(_import));
                listIterator.add(newImportMap);
            }
        }

        return objs;
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
            && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }
}