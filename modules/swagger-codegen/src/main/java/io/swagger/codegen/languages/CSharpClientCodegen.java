package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.models.properties.*;
import io.swagger.codegen.CliOption;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CSharpClientCodegen extends DefaultCodegen implements CodegenConfig {
    @SuppressWarnings({ "unused", "hiding" })
    private static final Logger LOGGER = LoggerFactory.getLogger(CSharpClientCodegen.class);
    protected boolean optionalAssemblyInfoFlag = true;
    protected boolean optionalProjectFileFlag = false;
    protected boolean optionalMethodArgumentFlag = true;
    protected boolean useDateTimeOffsetFlag = false;
    protected boolean useCollection = false;
    protected boolean returnICollection = false;
    protected String packageGuid = "{" + java.util.UUID.randomUUID().toString().toUpperCase() + "}";
    protected String packageTitle = "Swagger Library";
    protected String packageProductName = "SwaggerLibrary";
    protected String packageDescription = "A library generated from a Swagger doc";
    protected String packageCompany = "Swagger";
    protected String packageCopyright = "No Copyright";
    protected String packageName = "IO.Swagger";
    protected String packageVersion = "1.0.0";
    protected String clientPackage = "IO.Swagger.Client";
    protected String sourceFolder = "src" + File.separator + "main" + File.separator + "csharp";

    public CSharpClientCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "csharp";
        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");
        embeddedTemplateDir = templateDir = "csharp";
        apiPackage = "IO.Swagger.Api";
        modelPackage = "IO.Swagger.Model";

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
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "C# package name (convention: Camel.Case).")
                .defaultValue("IO.Swagger"));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION, "C# package version.").defaultValue("1.0.0"));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.OPTIONAL_METHOD_ARGUMENT, "C# Optional method argument, " +
                "e.g. void square(int x=10) (.net 4.0+ only)."));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.OPTIONAL_ASSEMBLY_INFO,
                CodegenConstants.OPTIONAL_ASSEMBLY_INFO_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC).defaultValue(sourceFolder));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.USE_DATETIME_OFFSET, CodegenConstants.USE_DATETIME_OFFSET_DESC));

        cliOptions.add( CliOption.newBoolean(CodegenConstants.USE_COLLECTION, CodegenConstants.USE_COLLECTION_DESC)
                 .defaultValue(Boolean.FALSE.toString()) );
        cliOptions.add( CliOption.newBoolean(CodegenConstants.RETURN_ICOLLECTION, CodegenConstants.RETURN_ICOLLECTION_DESC)
                .defaultValue(Boolean.FALSE.toString()) );
        cliOptions.add(CliOption.newBoolean(CodegenConstants.OPTIONAL_PROJECT_FILE,
                CodegenConstants.OPTIONAL_PROJECT_FILE_DESC).defaultValue(Boolean.FALSE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.OPTIONAL_PROJECT_GUID, CodegenConstants.OPTIONAL_PROJECT_GUID_DESC));      
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)){
            setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }
        else
        {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, this.sourceFolder);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
            apiPackage = packageName + ".Api";
            modelPackage = packageName + ".Model";
            clientPackage = packageName + ".Client";
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        // Use DateTimeOffset
        if (additionalProperties.containsKey(CodegenConstants.USE_DATETIME_OFFSET))
        {
            useDateTimeOffset(Boolean.valueOf(additionalProperties.get(CodegenConstants.USE_DATETIME_OFFSET).toString()));
        }
        additionalProperties.put(CodegenConstants.USE_DATETIME_OFFSET, useDateTimeOffsetFlag);

        additionalProperties.put("clientPackage", clientPackage);

        // Add properties used by AssemblyInfo.mustache
        additionalProperties.put("packageTitle", packageTitle);
        additionalProperties.put("packageProductName", packageProductName);
        additionalProperties.put("packageDescription", packageDescription);
        additionalProperties.put("packageCompany", packageCompany);
        additionalProperties.put("packageCopyright", packageCopyright);
        
        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_FILE))
        {
            setOptionalProjectFileFlag(Boolean.valueOf(
                additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_FILE).toString()));
        }
        
        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_PROJECT_GUID))
        {
            setPackageGuid((String) additionalProperties.get(CodegenConstants.OPTIONAL_PROJECT_GUID));
        }
		additionalProperties.put("packageGuid", packageGuid);
		
        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_METHOD_ARGUMENT)) {
            setOptionalMethodArgumentFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.OPTIONAL_METHOD_ARGUMENT).toString()));
        }
        additionalProperties.put("optionalMethodArgument", optionalMethodArgumentFlag);
        
        if (additionalProperties.containsKey(CodegenConstants.OPTIONAL_ASSEMBLY_INFO)) {
            setOptionalAssemblyInfoFlag(Boolean.valueOf(additionalProperties
                    .get(CodegenConstants.OPTIONAL_ASSEMBLY_INFO).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.USE_COLLECTION)){
            setUseCollection(Boolean.valueOf(additionalProperties.get(CodegenConstants.USE_COLLECTION).toString()));
        }

        if (additionalProperties.containsKey(CodegenConstants.RETURN_ICOLLECTION)){
            setReturnICollection(Boolean.valueOf(additionalProperties.get(CodegenConstants.RETURN_ICOLLECTION).toString()));
        }

        
        String packageFolder = sourceFolder + File.separator + packageName.replace(".", java.io.File.separator);
        String clientPackageDir = sourceFolder + File.separator + clientPackage.replace(".", java.io.File.separator);
        
        //Compute the relative path to the bin directory where the external assemblies live
        //This is necessary to properly generate the project file
        int packageDepth = packageFolder.length() - packageFolder.replace(java.io.File.separator, "").length();
        String binRelativePath = "..\\";
        for (int i=0; i < packageDepth; i = i+1)
            binRelativePath += "..\\";
        binRelativePath += "bin\\";
        additionalProperties.put("binRelativePath", binRelativePath);
        
        supportingFiles.add(new SupportingFile("Configuration.mustache",
                clientPackageDir, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                clientPackageDir, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                clientPackageDir, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("ApiResponse.mustache",
                clientPackageDir, "ApiResponse.cs"));
				
        supportingFiles.add(new SupportingFile("Newtonsoft.Json.dll", "bin", "Newtonsoft.Json.dll"));
        supportingFiles.add(new SupportingFile("RestSharp.dll", "bin", "RestSharp.dll"));
        supportingFiles.add(new SupportingFile("compile.mustache", "", "compile.bat"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "compile-mono.sh"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", "vendor" + java.io.File.separator, "packages.config"));
        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));

        if (optionalAssemblyInfoFlag) {
            supportingFiles.add(new SupportingFile("AssemblyInfo.mustache", packageFolder + File.separator + "Properties", "AssemblyInfo.cs"));
        }
        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Project.mustache", packageFolder, clientPackage + ".csproj"));
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "csharp";
    }

    @Override
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

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

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
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        super.postProcessOperations(objs);
        if(objs != null) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.returnType != null) {
                        operation.returnContainer = operation.returnType;
                        if( this.returnICollection && (
                                operation.returnType.startsWith("List")||
                                operation.returnType.startsWith("Collection")) ) {
                            // NOTE: ICollection works for both List<T> and Collection<T>
                            int genericStart = operation.returnType.indexOf("<");
                            if(genericStart > 0) {
                                operation.returnType = "ICollection" + operation.returnType.substring(genericStart);
                            }
                        }
                    }
                }
            }
        }

        return objs;
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

    public void setOptionalAssemblyInfoFlag(boolean flag) {
        this.optionalAssemblyInfoFlag = flag;
    }

    public void setOptionalProjectFileFlag(boolean flag) {
        this.optionalProjectFileFlag = flag;
    }
	
    public void setOptionalMethodArgumentFlag(boolean flag) {
        this.optionalMethodArgumentFlag = flag;
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

    public void useDateTimeOffset(boolean flag) {
        this.useDateTimeOffsetFlag = flag;
        if (flag)
            typeMapping.put("datetime", "DateTimeOffset?");
        else
            typeMapping.put("datetime", "DateTime?");
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
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
}
