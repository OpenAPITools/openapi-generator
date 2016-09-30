package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.codegen.examples.ExampleGenerator;
import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Response;
import io.swagger.models.Swagger;
import io.swagger.models.properties.*;

import java.util.*;
import java.io.File;

public class CppRestClientCodegen extends DefaultCodegen implements CodegenConfig {

    public static final String DECLSPEC = "declspec";
    public static final String DEFAULT_INCLUDE = "defaultInclude";

    protected String packageVersion = "1.0.0";
    protected String declspec = "";
    protected String defaultInclude = "";

    /**
     * Configures the type of generator.
     * 
     * @return the CodegenType for this generator
     * @see io.swagger.codegen.CodegenType
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -l flag.
     * 
     * @return the friendly name for the generator
     */
    public String getName() {
        return "cpprest";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with
     * help tips, parameters here
     * 
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a C++ API client with C++ REST SDK (https://github.com/Microsoft/cpprestsdk).";
    }

    public CppRestClientCodegen() {
        super();

        apiPackage = "io.swagger.client.api";
        modelPackage = "io.swagger.client.model";

        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");

        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        templateDir = "cpprest";

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.MODEL_PACKAGE, "C++ namespace for models (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "C++ namespace for apis (convention: name.space.api).",
                this.apiPackage);
        addOption(CodegenConstants.PACKAGE_VERSION, "C++ package version.", this.packageVersion);
        addOption(DECLSPEC, "C++ preprocessor to place before the class name for handling dllexport/dllimport.",
                this.declspec);
        addOption(DEFAULT_INCLUDE,
                "The default include statement that should be placed in all headers for including things like the declspec (convention: #include \"Commons.h\" ",
                this.defaultInclude);

        reservedWords = new HashSet<String>();

        supportingFiles.add(new SupportingFile("modelbase-header.mustache", "", "ModelBase.h"));
        supportingFiles.add(new SupportingFile("modelbase-source.mustache", "", "ModelBase.cpp"));
        supportingFiles.add(new SupportingFile("apiclient-header.mustache", "", "ApiClient.h"));
        supportingFiles.add(new SupportingFile("apiclient-source.mustache", "", "ApiClient.cpp"));
        supportingFiles.add(new SupportingFile("apiconfiguration-header.mustache", "", "ApiConfiguration.h"));
        supportingFiles.add(new SupportingFile("apiconfiguration-source.mustache", "", "ApiConfiguration.cpp"));
        supportingFiles.add(new SupportingFile("apiexception-header.mustache", "", "ApiException.h"));
        supportingFiles.add(new SupportingFile("apiexception-source.mustache", "", "ApiException.cpp"));
        supportingFiles.add(new SupportingFile("ihttpbody-header.mustache", "", "IHttpBody.h"));
        supportingFiles.add(new SupportingFile("jsonbody-header.mustache", "", "JsonBody.h"));
        supportingFiles.add(new SupportingFile("jsonbody-source.mustache", "", "JsonBody.cpp"));
        supportingFiles.add(new SupportingFile("httpcontent-header.mustache", "", "HttpContent.h"));
        supportingFiles.add(new SupportingFile("httpcontent-source.mustache", "", "HttpContent.cpp"));
        supportingFiles.add(new SupportingFile("multipart-header.mustache", "", "MultipartFormData.h"));
        supportingFiles.add(new SupportingFile("multipart-source.mustache", "", "MultipartFormData.cpp"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "utility::datetime");
        typeMapping.put("DateTime", "utility::datetime");
        typeMapping.put("string", "utility::string_t");
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "HttpContent");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "std::string");

        super.importMapping = new HashMap<String, String>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("HttpContent", "#include \"HttpContent.h\"");
        importMapping.put("Object", "#include \"Object.h\"");
        importMapping.put("utility::string_t", "#include <cpprest/details/basic_types.h>");
        importMapping.put("utility::datetime", "#include <cpprest/details/basic_types.h>");
    }

    protected void addOption(String key, String description, String defaultValue) {
        CliOption option = new CliOption(key, description);
        if (defaultValue != null)
            option.defaultValue(defaultValue);
        cliOptions.add(option);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(DECLSPEC)) {
            declspec = additionalProperties.get(DECLSPEC).toString();
        }

        if (additionalProperties.containsKey(DEFAULT_INCLUDE)) {
            defaultInclude = additionalProperties.get(DEFAULT_INCLUDE).toString();
        }

        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
        additionalProperties.put("declspec", declspec);
        additionalProperties.put("defaultInclude", defaultInclude);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reseved words
     * 
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return "_" + name; // add an underscore to the name
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    public String modelFileFolder() {
        return outputFolder + "/model";
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/api";
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".h\"";
        }
    }

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel codegenModel = super.fromModel(name, model, allDefinitions);

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<String>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }

        return codegenModel;
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation,
            Map<String, Model> definitions, Swagger swagger) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, definitions, swagger);

        if (operation.getResponses() != null && !operation.getResponses().isEmpty()) {
            Response methodResponse = findMethodResponse(operation.getResponses());

            if (methodResponse != null) {
                if (methodResponse.getSchema() != null) {
                    CodegenProperty cm = fromProperty("response", methodResponse.getSchema());
                    op.vendorExtensions.put("x-codegen-response", cm);
                    if(cm.datatype == "HttpContent")
                    {
                        op.vendorExtensions.put("x-codegen-response-ishttpcontent", true);
                    }
                }
            }
        }

        return op;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (isFileProperty(property)) {
            property.vendorExtensions.put("x-codegen-file", true);
        }
    }

    protected boolean isFileProperty(CodegenProperty property) {
        return property.baseType.equals("HttpContent");
    }

    @Override
    public String toModelFilename(String name) {
        return initialCaps(name);
    }

    @Override
    public String toApiFilename(String name) {
        return initialCaps(name) + "Api";
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     *         `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Property p) {
        String swaggerType = getSwaggerType(p);

        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        }
        if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getSwaggerType(p) + "<utility::string_t, " + getTypeDeclaration(inner) + ">";
        }
        if (p instanceof StringProperty || p instanceof DateProperty
                || p instanceof DateTimeProperty || p instanceof FileProperty
                || languageSpecificPrimitives.contains(swaggerType)) {
            return toModelName(swaggerType);
        }

        return "std::shared_ptr<" + swaggerType + ">";
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            return "U(\"\")";
        } else if (p instanceof BooleanProperty) {
            return "false";
        } else if (p instanceof DateProperty) {
            return "utility::datetime()";
        } else if (p instanceof DateTimeProperty) {
            return "utility::datetime()";
        } else if (p instanceof DoubleProperty) {
            return "0.0";
        } else if (p instanceof FloatProperty) {
            return "0.0f";
        } else if (p instanceof IntegerProperty || p instanceof BaseIntegerProperty) {
            return "0";
        } else if (p instanceof LongProperty) {
            return "0L";
        } else if (p instanceof DecimalProperty) {
            return "0.0";
        } else if (p instanceof MapProperty) {
            MapProperty ap = (MapProperty) p;
            String inner = getSwaggerType(ap.getAdditionalProperties());
            return "std::map<utility::string_t, " + inner + ">()";
        } else if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            if (!languageSpecificPrimitives.contains(inner)) {
                inner = "std::shared_ptr<" + inner + ">";
            }
            return "std::vector<" + inner + ">()";
        } else if (p instanceof RefProperty) {
            RefProperty rp = (RefProperty) p;
            return "new " + toModelName(rp.getSimpleRef()) + "()";
        }
        return "nullptr";
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isListContainer = parameter.isListContainer == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;

        if (!isPrimitiveType && !isListContainer && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
            parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
        }
    }

    /**
     * Optional - swagger type conversion. This is used to map swagger types in
     * a `Property` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
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
        } else
            type = swaggerType;
        return toModelName(type);
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) || typeMapping.values().contains(type)
                || importMapping.values().contains(type) || defaultIncludes.contains(type)
                || languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toVarName(String name) {
        if (typeMapping.keySet().contains(name) || typeMapping.values().contains(name)
                || importMapping.values().contains(name) || defaultIncludes.contains(name)
                || languageSpecificPrimitives.contains(name)) {
            return name;
        }

        if (name.length() > 1) {
            return Character.toUpperCase(name.charAt(0)) + name.substring(1);
        }

        return name;
    }

    @Override
    public String toApiName(String type) {
        return Character.toUpperCase(type.charAt(0)) + type.substring(1) + "Api";
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

}
