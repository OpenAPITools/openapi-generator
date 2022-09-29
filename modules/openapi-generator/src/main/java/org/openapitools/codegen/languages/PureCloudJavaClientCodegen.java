package org.openapitools.codegen.languages;

import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.templating.mustache.PrefixNumberWithValue;
import org.openapitools.codegen.templating.mustache.StringReplaceLambda;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class PureCloudJavaClientCodegen extends JavaClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudJavaClientCodegen.class);

    public PureCloudJavaClientCodegen() {
        super();

        this.setInvokerPackage("com.mypurecloud.sdk.v2");
        this.setApiPackage("com.mypurecloud.sdk.v2.api");
        this.setModelPackage("com.mypurecloud.sdk.v2.model");

        // Use default templates
        embeddedTemplateDir = templateDir = "Java";

        // Custom mappings for swagger type -> java type
        importMapping.put("LocalDateTime", "org.joda.time.LocalDateTime");
        importMapping.put("PagedResource", "com.mypurecloud.sdk.v2.PagedResource");
        importMapping.put("ArrayNode", "com.fasterxml.jackson.databind.node.ArrayNode");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("JsonNode", "com.fasterxml.jackson.databind.JsonNode");
        importMapping.put("LinkedHashSet", "java.util.ArrayList");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("ArrayList", "java.util.ArrayList");
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("Map", "java.util.Map");
        importMapping.put("BigDecimal", "java.math.BigDecimal");

        // Type overrides
        typeMapping.put("date", "LocalDate");
        typeMapping.put("URI", "String");
        typeMapping.put("set", "List");
        typeMapping.put("empty", "Object");
        typeMapping.put("Empty", "Object");

        // Add special reserved words
        reservedWords.add("null");
        reservedWords.add("request");

        operationTemplateFiles.put("requestBuilder.mustache", ".java");
        //supportingFiles.add(new SupportingFile("testng.mustache", "", "testng.xml"));
        apiDocTemplateFiles.put("api_json.mustache", ".json");
        supportingFiles.add(new SupportingFile("testng-unit.mustache", "", "testng-unit.xml").doNotOverwrite());
        supportingFiles.add(new SupportingFile("testng-integration.mustache", "", "testng-integration.xml").doNotOverwrite());
        //apiDocTemplateFiles.put("api_json.mustache", ".json");
        operationTemplateFiles.put("operation_example.mustache", "-example.txt");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, "com.mypurecloud.sdk.v2");

        setRemoveEnumValuePrefix(false);
    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        for (CodegenParameter cp : op.allParams) {
            if (cp.dataType.equals("Map<String, Object>")) {
                cp.dataType = "Object";
            }
        }

        Set<String> imports = new HashSet<>();
        for (String im : op.imports) {
            if (isReservedWord(im)) {
                im = "Model" + im;
            }
            imports.add(im);
        }
        op.imports = imports;

        if (op.hasRequiredParams) {
            for (CodegenParameter cp : op.requiredParams) {
                if (cp.dataType.equals("Map<String, Object>")) {
                    cp.dataType = "Object";
                }
            }
        }

        if (op.bodyParam != null && op.bodyParam.dataType != null && op.bodyParam.dataType.equals("Map<String, Object>")) {
            op.bodyParam.dataType = "Object";
        }

        if (op.returnType != null && op.returnType.equals("Object") && op.responses.size() > 0 && op.responses.get(0).jsonSchema.contains("#/components/schemas/Empty")) {
            op.returnType = "Empty";
        }

        if (op.returnType != null && op.responses.get(0).jsonSchema.contains("#/components/schemas/Configuration")) {
            op.returnType = "ModelConfiguration";
        }

        return op;
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        if (objs == null) return super.postProcessOperationsWithModels(objs, allModels);

        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                for (CodegenParameter param : operation.allParams) {
                    Map<String, Object> allowableValues = param.allowableValues;
                    if (allowableValues != null) {
                        param.allowableValuesForEnum = new HashMap<>();
                        for (Map.Entry<String, Object> value : allowableValues.entrySet()) {
                            List<CodegenParameter.Tuple<String, String>> formattedValues = new ArrayList<>();
                            for (Object val : (ArrayList<String>)value.getValue()) {
                                if (val instanceof String) {
                                    formattedValues.add(new CodegenParameter.Tuple<>("item1", "item2"));
                                }

                            }
                            param.allowableValuesForEnum.put("values", formattedValues);
                        }
                    }
                }
            }
        }

        return super.postProcessOperationsWithModels(objs, allModels);
    }

    @Override
    public String getName() { return "purecloudjava"; }

    @Override
    /**
     * Get the operation ID or use default behavior if blank.
     *
     * @param operation the operation object
     * @param path the path of the operation
     * @param httpMethod the HTTP method of the operation
     * @return the (generated) operationId
     */
    protected String getOrGenerateOperationId(Operation operation, String path, String httpMethod) {
        if (operation.getExtensions().containsKey(OPERATION_ID_PROPERTY_NAME)) {
            String operationId = operation.getExtensions().get(OPERATION_ID_PROPERTY_NAME).toString();
            if (!StringUtils.isBlank(operationId)) {
                return operationId;
            }
        }

        return super.getOrGenerateOperationId(operation, path, httpMethod);
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);

        List<Object> models = (List<Object>) objs.get("models");

        // Iterate through models
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            List<CodegenProperty> vars = new ArrayList<>();
            // Iterate through properties
            for (CodegenProperty cp : cm.allVars) {
                // Enums with values only
                if (cp.isEnum && cp.allowableValues != null) {
                    Object valuesObject = cp.allowableValues.get("values");
                    if (valuesObject != null) {
                        ArrayList valuesArray = (ArrayList) valuesObject;
                        if (valuesArray.get(0) instanceof Integer) {
                            // Integer enum type
                            valuesArray.add(0, -1);
                            Object enumVarsObject = cp.allowableValues.get("enumVars");
                            ArrayList enumVarsArray = (ArrayList) enumVarsObject;
                            HashMap<String, String> newItem = new HashMap<>();
                            newItem.put("name", "OUTDATEDSDKVERSION");
                            newItem.put("value", toEnumValue("-1", "Integer"));
                            enumVarsArray.add(0, newItem);
                        } else {
                            // String enum type
                            if (!valuesArray.get(0).equals("OutdatedSdkVersion")) {
                                valuesArray.add(0, "OutdatedSdkVersion");
                                Object enumVarsObject = cp.allowableValues.get("enumVars");
                                ArrayList enumVarsArray = (ArrayList) enumVarsObject;
                                HashMap<String, String> newItem = new HashMap<>();
                                newItem.put("name", "OUTDATEDSDKVERSION");
                                newItem.put("isString", "true");
                                newItem.put("value", toEnumValue("OutdatedSdkVersion", "String"));
                                enumVarsArray.add(0, newItem);
                            }
                        }
                    }
                }
                vars.add(cp);
            }
            cm.allVars = vars;

        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if (property.example != null) {
            property.example = property.example.replace("\\\"", "");
        }

        if (property.description != null) {
            // [DEVENGAGE-345] Escape */* in description - this interferes with javadoc comments and annotations - transforms */* -> *\\/\\*
            property.description = property.description.replaceAll("\\*/\\*","*\\\\\\\\/\\\\\\\\*");
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        CodegenModel codegenModel = super.fromModel(name, schema);

        for (CodegenProperty cp : codegenModel.vars) {
            cp.datatypeWithEnum = cp.datatypeWithEnum.replaceAll("_+$", "");
        }

        Set<String> imports = new HashSet<>();
        for (String im : codegenModel.imports) {
            imports.add(im.replaceAll("_+$", ""));
        }
        codegenModel.imports = imports;

        codegenModel.isPagedResource = true;

        for (CodegenProperty var : codegenModel.vars) {
            var.datatypeWithEnum = var.datatypeWithEnum
                    .replaceAll("_+$", "")
                    .replaceAll("_+(>)", "$1");
            var.defaultValue = var.defaultValue
                    .replaceAll("_+$", "")
                    .replaceAll("_+(>)", "$1");
        }

        for (String s : Arrays.asList("pageSize","pageNumber","total","selfUri","firstUri","previousUri","nextUri","lastUri","pageCount", "entities")) {
            if (codegenModel.allVars.stream().noneMatch(var -> var.name.equals(s))) {
                codegenModel.isPagedResource = false;
                break;
            }
        }

        if (codegenModel.isPagedResource) {
            // Get reference to entities property
            Optional<CodegenProperty> entitiesProperty = codegenModel.allVars.stream().filter(var -> var.name.equals("entities")).findFirst();
            if (!entitiesProperty.isPresent()) {
                return codegenModel;
            }

            codegenModel.removeAllDuplicatedProperty();

            // datatypeWithEnum has the correct type including generics. complexType drops them.
            // E.g. datatypeWithEnum=Map<Object, String> and complexType=Map
            codegenModel.pagedResourceType = entitiesProperty.get().datatypeWithEnum;
            if (codegenModel.pagedResourceType.startsWith("List")) {
                codegenModel.pagedResourceType = codegenModel.pagedResourceType.substring(5, codegenModel.pagedResourceType.length() - 1);
            }
            codegenModel.imports.add("PagedResource");
        }

        codegenModel.removeAllDuplicatedProperty();

        return codegenModel;
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (ModelUtils.isMapSchema(schema)) {
            // API-2916 default values for Map properties cause unexpected issues for PUT requests
            return "null";
        } else if (ModelUtils.isArraySchema(schema)) {
            // BUG FIX Lists default value should be null
            return "null";
        } else {
            return super.toDefaultValue(schema);
        }
    }

    @Override
    protected ImmutableMap.Builder<String, Mustache.Lambda> addMustacheLambdas() {
        HashMap<String, String> replaceMap = new HashMap<>();
        replaceMap.put(".", "_");
        replaceMap.put("-", "_");
        replaceMap.put("/", "_");
        replaceMap.put(":", "_");
        replaceMap.put("__", "_");

        return super.addMustacheLambdas()
                .put("replace", new StringReplaceLambda(replaceMap))
                .put("prefixNumberWithUnderscore", new PrefixNumberWithValue("_", false))
                .put("prefixNumberWithWord", new PrefixNumberWithValue("NUMBER_", true));
    }
}
