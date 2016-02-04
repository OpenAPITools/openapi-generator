package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.properties.*;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class AspNet5ServerCodegen extends DefaultCodegen implements CodegenConfig {
    protected boolean useDateTimeOffsetFlag = false;
    protected String packageName = "IO.Swagger";
    protected String packageVersion = "1.0.0";
    protected boolean useCollection = false;
    protected boolean returnICollection = false;
    protected String sourceFolder = "src" + File.separator + packageName;

    protected Set<String> collectionTypes;
    protected Set<String> mapTypes;

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNet5ServerCodegen.class);

    public AspNet5ServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "aspnet5";
        embeddedTemplateDir = templateDir = "aspnet5";

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("controller.mustache", ".cs");

        // TODO: Create a base C# abstract type to avoid duplication of language functionality
        collectionTypes = new HashSet<String>(
                Arrays.asList(
                        "IList", "List",
                        "ICollection", "Collection",
                        "IEnumerable")
        );
        mapTypes = new HashSet<String>(
                Arrays.asList("IDictionary")
        );

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        // local variable names in API methods (endpoints)
                        "path_", "pathParams", "queryParams", "headerParams", "formParams", "fileParams",
                        "postBody", "http_header_accepts", "http_header_accept", "apiKeyValue", "response",
                        "statusCode",
                        // C# reserved words
                        "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked",
                        "class", "const", "continue", "decimal", "default", "delegate", "do", "double", "else",
                        "enum", "event", "explicit", "extern", "false", "finally", "fixed", "float", "for",
                        "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock",
                        "long", "namespace", "new", "null", "object", "operator", "out", "override", "params",
                        "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed",
                        "short", "sizeof", "stackalloc", "static", "string", "struct", "switch", "this", "throw",
                        "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort", "using",
                        "virtual", "void", "volatile", "while")
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
                        "ICollection",
                        "Collection",
                        "List",
                        "Dictionary",
                        "DateTime?",
                        "DateTimeOffset?",
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
        instantiationTypes.put("list", "List");
        instantiationTypes.put("map", "Dictionary");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "string");
        typeMapping.put("binary", "byte[]");
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

        addOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Title.Case).",
                this.packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.",
                this.packageVersion);

        addOption(CodegenConstants.SOURCE_FOLDER,
                CodegenConstants.SOURCE_FOLDER_DESC,
                sourceFolder);

        addSwitch(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC,
                Boolean.TRUE);

        addSwitch(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC,
                Boolean.TRUE);

        addSwitch(CodegenConstants.USE_DATETIME_OFFSET,
                CodegenConstants.USE_DATETIME_OFFSET_DESC,
                Boolean.FALSE);

        addSwitch(CodegenConstants.USE_COLLECTION,
                CodegenConstants.USE_COLLECTION_DESC,
                Boolean.FALSE);

        addSwitch(CodegenConstants.RETURN_ICOLLECTION,
                CodegenConstants.RETURN_ICOLLECTION_DESC,
                Boolean.FALSE);
    }

    private void addOption(String key, String description, String defaultValue){
        CliOption option = new CliOption(key, description);
        if(defaultValue != null) option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    private void addSwitch(String key, String description, Boolean defaultValue){
        CliOption option = CliOption.newBoolean(key, description);
        if(defaultValue != null) option.defaultValue(defaultValue.toString());
        cliOptions.add(option);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // {{packageVersion}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        // {{sourceFolder}}
        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)){
            setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        } else {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, this.sourceFolder);
        }

        // {{packageName}}
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        apiPackage = packageName + ".Controllers";
        modelPackage = packageName + ".Models";

        // {{useDateTimeOffset}}
        if (additionalProperties.containsKey(CodegenConstants.USE_DATETIME_OFFSET))
        {
            useDateTimeOffset(Boolean.valueOf(additionalProperties.get(CodegenConstants.USE_DATETIME_OFFSET).toString()));
        }
        additionalProperties.put(CodegenConstants.USE_DATETIME_OFFSET, useDateTimeOffsetFlag);

        if (additionalProperties.containsKey(CodegenConstants.USE_COLLECTION)){
            setUseCollection(Boolean.valueOf(additionalProperties.get(CodegenConstants.USE_COLLECTION).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.RETURN_ICOLLECTION)){
            setReturnICollection(Boolean.valueOf(additionalProperties.get(CodegenConstants.RETURN_ICOLLECTION).toString()));
        }

        supportingFiles.add(new SupportingFile("global.json", "", "global.json"));
        supportingFiles.add(new SupportingFile("build.mustache", "", "build.sh"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", this.sourceFolder, "Dockerfile"));
        supportingFiles.add(new SupportingFile("gitignore", this.sourceFolder, ".gitignore"));
        supportingFiles.add(new SupportingFile("appsettings.json", this.sourceFolder, "appsettings.json"));

        supportingFiles.add(new SupportingFile("project.mustache", this.sourceFolder, "project.json"));
        supportingFiles.add(new SupportingFile("Startup.mustache", this.sourceFolder, "Startup.cs"));

        supportingFiles.add(new SupportingFile("Properties"+File.separator +"launchSettings.json", this.sourceFolder + File.separator + "Properties", "launchSettings.json"));

        supportingFiles.add(new SupportingFile("wwwroot" +File.separator + "README.md", this.sourceFolder + File.separator + "wwwroot", "README.md"));
        supportingFiles.add(new SupportingFile("wwwroot" +File.separator + "index.html", this.sourceFolder + File.separator + "wwwroot", "index.html"));
        supportingFiles.add(new SupportingFile("wwwroot" +File.separator + "web.config", this.sourceFolder + File.separator + "wwwroot", "web.config"));
    }

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        if (flag) typeMapping.put("datetime", "DateTimeOffset?");
        else typeMapping.put("datetime", "DateTime?");
    }

    public void setReturnICollection(boolean returnICollection) {
        this.returnICollection = returnICollection;
    }

    public void setUseCollection(boolean useCollection) {
        this.useCollection = useCollection;
        if(useCollection){
            typeMapping.put("array", "Collection");
            typeMapping.put("list", "Collection");

            instantiationTypes.put("array", "Collection");
            instantiationTypes.put("list", "Collection");
        }
    }

    @Override
    public CodegenType getTag() { return CodegenType.SERVER; }

    @Override
    public String getName() { return "aspnet5"; }

    @Override
    public String getHelp() {
        return "Generates an ASP.NET 5 Web API server.";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "Controllers";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator +  "Models";
    }

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

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

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
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

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    @Override
    public String toModelName(String name) {
        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(name);
    }

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
    }

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        super.postProcessOperations(objs);
        if(objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    // HACK: Unlikely in the wild, but we need to clean operation paths for MVC Routing
                    if(operation.path != null){
                        String original = operation.path;
                        operation.path = operation.path.replace("?", "/");
                        if(!original.equals(operation.path)){
                            LOGGER.warn("Normalized " + original + " to " + operation.path + ". Please verify generated source.");
                        }
                    }

                    // Check return types for collection
                    if (operation.returnType != null) {
                        String typeMapping;
                        int namespaceEnd = operation.returnType.lastIndexOf(".");
                        if(namespaceEnd > 0) {
                            typeMapping = operation.returnType.substring(namespaceEnd);
                        } else {
                            typeMapping = operation.returnType;
                        }

                        if(this.collectionTypes.contains(typeMapping)){
                            operation.isListContainer = true;
                            operation.returnContainer = operation.returnType;
                            if( this.returnICollection && (
                                    typeMapping.startsWith("List")||
                                            typeMapping.startsWith("Collection")) ) {
                                // NOTE: ICollection works for both List<T> and Collection<T>
                                int genericStart = typeMapping.indexOf("<");
                                if(genericStart > 0) {
                                    operation.returnType = "ICollection" + typeMapping.substring(genericStart);
                                }
                            }
                        } else {
                            operation.returnContainer = operation.returnType;
                            operation.isMapContainer = this.mapTypes.contains(typeMapping);
                        }
                    }

                    // Converts, for example, PUT to HttpPut for controller attributes
                    operation.httpMethod = "Http" + operation.httpMethod.substring(0,1) + operation.httpMethod.substring(1).toLowerCase();
                }
            }
        }

        return objs;
    }

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
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

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
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

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
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

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            for (CodegenProperty var : cm.vars) {
                // check to see if model name is same as the property name
                // which will result in compilation error
                // if found, prepend with _ to workaround the limitation
                if (var.name.equals(cm.name)) {
                    var.name = "_" + var.name;
                }
            }
        }
        return objs;
    }

    // TODO: Create a base C# abstract type to avoid duplication of language functionality
    /**
     * Return the default value of the property
     *
     * @param p Swagger property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            StringProperty dp = (StringProperty) p;
            if (dp.getDefault() != null) {
                return "\"" + dp.getDefault().toString() + "\"";
            }
        } else if (p instanceof BooleanProperty) {
            BooleanProperty dp = (BooleanProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof DateProperty) {
            // TODO
        } else if (p instanceof DateTimeProperty) {
            // TODO
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof FloatProperty) {
            FloatProperty dp = (FloatProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof IntegerProperty) {
            IntegerProperty dp = (IntegerProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        } else if (p instanceof LongProperty) {
            LongProperty dp = (LongProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
        }

        return null;
    }


    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }
}
