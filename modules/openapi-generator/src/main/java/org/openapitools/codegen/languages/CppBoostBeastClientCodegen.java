package org.openapitools.codegen.languages;


import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;

import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppBoostBeastClientCodegen extends AbstractCppCodegen {

    static final Logger LOGGER = LoggerFactory.getLogger(CppBoostBeastClientCodegen.class);

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "cpp-boost-beast-client";
    }

    public String getHelp() {
        return "Generates a cpp-boost-beast client.";
    }

    public CppBoostBeastClientCodegen() {
        super();
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "cpp-boost-beast";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-boost-beast-client";

        modelPackage = "org.openapitools.client.model";
        apiPackage = "org.openapitools.client.api";

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.MODEL_PACKAGE, "C++ namespace for models (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "C++ namespace for apis (convention: name.space.api).",
                this.apiPackage);


        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("http-client-header.mustache", "api", "HttpClient.h"));
        supportingFiles.add(new SupportingFile("http-client-impl-header.mustache", "api", "HttpClientImpl.h"));
        supportingFiles.add(new SupportingFile("http-client-impl-source.mustache", "api", "HttpClientImpl.cpp"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        super.typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "Object");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
        typeMapping.put("ByteArray", "std::string");

        super.importMapping = new HashMap<String, String>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("int32_t", "#include <cstdint>");
        importMapping.put("int64_t", "#include <cstdint>");
    }


    @Override
    public Map<String, ModelsMap> updateAllModels(Map<String, ModelsMap> objs)  {
        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = getAllModels(objs);

        // Clean interfaces of ambiguity
        for (Map.Entry<String, CodegenModel> cm : allModels.entrySet()) {
            if (cm.getValue().getInterfaces() != null && !cm.getValue().getInterfaces().isEmpty()) {
                List<String> newIntf = new ArrayList<>(cm.getValue().getInterfaces());

                for (String intf : allModels.get(cm.getKey()).getInterfaces()) {
                    if (allModels.get(intf).getInterfaces() != null && !allModels.get(intf).getInterfaces().isEmpty()) {
                        for (String intfInner : allModels.get(intf).getInterfaces()) {
                            newIntf.remove(intfInner);
                        }
                    }
                }
                cm.getValue().setInterfaces(newIntf);
            }
        }

        objs = super.updateAllModels(objs);
        return objs;
    }

    /**
     * Camelize the method name of the getter and setter, but keep underscores at the front
     *
     * @param name string to be camelized
     * @return Camelized string
     */
    @Override
    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }

        name = toVarName(name);

        if (name.startsWith("_")) {
            return "_" + camelize(name);
        }

        return camelize(name);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    @Override
    public String modelFileFolder() {
        return (outputFolder + "/model").replace("/", File.separator);
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
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
    public CodegenModel fromModel(String name, Schema model) {
        CodegenModel codegenModel = super.fromModel(name, model);

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }
        // Import vector if an enum is present
        if (codegenModel.hasEnums) {
            codegenModel.imports.add("#include <vector>");
        }
        return codegenModel;
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    @SuppressWarnings("unchecked")
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        List<CodegenOperation> newOpList = new ArrayList<>();

        for (CodegenOperation op : operationList) {
            String path = op.path;

            String[] items = path.split("/", -1);
            String resourceNameCamelCase = "";
            op.path = "";
            int pathParamIndex = 0;
            for (String item : items) {
                if (item.length() > 1) {
                    if (item.matches("^\\{(.*)\\}$")) {
                        String tmpResourceName = item.substring(1, item.length() - 1);
                        resourceNameCamelCase += Character.toUpperCase(tmpResourceName.charAt(0)) + tmpResourceName.substring(1);
                        item = '%' + String.valueOf(pathParamIndex + 1) + '%';
                        ++pathParamIndex;
                    } else {
                        resourceNameCamelCase += Character.toUpperCase(item.charAt(0)) + item.substring(1);
                    }
                } else if (item.length() == 1) {
                    resourceNameCamelCase += Character.toUpperCase(item.charAt(0));
                }
                op.path += item + "/";
            }
            op.path = op.path.replaceFirst("/$", "");

            op.vendorExtensions.put("x-codegen-resource-name", resourceNameCamelCase);

            boolean foundInNewList = false;
            for (CodegenOperation op1 : newOpList) {
                if (!foundInNewList) {
                    if (op1.path.equals(op.path)) {
                        foundInNewList = true;
                        final String X_CODEGEN_OTHER_METHODS = "x-codegen-other-methods";
                        List<CodegenOperation> currentOtherMethodList = (List<CodegenOperation>) op1.vendorExtensions.get(X_CODEGEN_OTHER_METHODS);
                        if (currentOtherMethodList == null) {
                            currentOtherMethodList = new ArrayList<>();
                        }
                        op.operationIdCamelCase = op1.operationIdCamelCase;
                        currentOtherMethodList.add(op);
                        op1.vendorExtensions.put(X_CODEGEN_OTHER_METHODS, currentOtherMethodList);
                    }
                }
            }
            if (!foundInNewList) {
                newOpList.add(op);
            }
        }
        operations.put("operation", newOpList);
        return objs;
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     * `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "<std::string, " + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isByteArraySchema(p)) {
            return "std::string";
        } else if (ModelUtils.isStringSchema(p)
                || ModelUtils.isDateSchema(p)
                || ModelUtils.isDateTimeSchema(p) || ModelUtils.isFileSchema(p)
                || languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        }

        return "std::shared_ptr<" + openAPIType + ">";
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "false";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) { // float
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "f";
                } else {
                    return "0.0f";
                }
            } else { // double
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0.0";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (ModelUtils.isLongSchema(p)) { // long
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "L";
                } else {
                    return "0L";
                }
            } else { // integer
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0";
                }
            }
        } else if (ModelUtils.isByteArraySchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(getAdditionalProperties(p));
            return "std::map<std::string, " + inner + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            String inner = getSchemaType(ap.getItems());
            if (!languageSpecificPrimitives.contains(inner)) {
                inner = "std::shared_ptr<" + inner + ">";
            }
            return "std::vector<" + inner + ">()";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return "std::make_shared<" + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + ">()";
        }

        return "nullptr";
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isArray = parameter.isArray == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;

        if (!isPrimitiveType && !isArray && !isString && !parameter.dataType.startsWith("std::shared_ptr")) {
            parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
            parameter.defaultValue = "std::make_shared<" + parameter.dataType + ">()";
        }
    }

    /**
     * Optional - OpenAPI type conversion. This is used to map OpenAPI types in
     * a `Schema` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        String modelName;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
        } else {
            type = openAPIType;
        }

        modelName = toModelName(type);
        return modelName;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // Remove prefix added by DefaultCodegen
        String originalDefaultValue = var.defaultValue;
        super.updateCodegenPropertyEnum(var);
        var.defaultValue = originalDefaultValue;
    }
}
