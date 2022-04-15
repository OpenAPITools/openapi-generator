package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class PureCloudGoClientCodegen extends GoClientCodegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";
    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudGoClientCodegen.class);

    public PureCloudGoClientCodegen() {
        super();
        // Use default templates
        embeddedTemplateDir = templateDir = "go";
        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));
        // Go type for arbitrary objects
        // Mainly used for API types of Map<string, Object>, which are objects with additional properties of type object
        typeMapping.put("object", "interface{}");
        typeMapping.put("LocalDateTime", "time.Time");
    }

    @Override
    public String getName() {
        return "purecloudgo";
    }

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

    public String apiFileFolder() {
        return (outputFolder + "/platformclientv2").replace('/', File.separatorChar);
    }

    public String modelFileFolder() {
        return (outputFolder + "/platformclientv2").replace('/', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/docs").replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/docs").replace('/', File.separatorChar);
    }

    @Override
    public String toModelFilename(String name) {
        return super.toModelFilename(name).replaceAll("_", "").toLowerCase(Locale.getDefault());
    }

    @Override
    public String toApiFilename(String name) {
        return super.toApiFilename(name).replaceAll("_", "").toLowerCase(Locale.getDefault());
    }

    @Override
    public String escapeReservedWord(String name) {
        return "Var" + name;
    }

    @Override
    public String toVarName(String name) {
        // Escape invalid names
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            // camelize first char of keywords, e.g type -> Type, then escape. e.g Type -> VarType
            name = camelize(name);
            name = escapeReservedWord(name);
        }
        // replace non-alphanumeric with underscore
        name = name.replaceAll("[^a-zA-Z0-9]", "_");
        // camelize (lower first character) the variable name
        // pet_id => PetId
        name = camelize(name);
        // Full strip
        name = name.replaceAll("[^a-zA-Z0-9]", "");
        return name;
    }

    @Override
    public String getTypeDeclaration(Schema s) {
        if (ModelUtils.isArraySchema(s)) {
            ArraySchema arraySchema = (ArraySchema) s;
            Schema inner = arraySchema.getItems();
            return "[]" + getTypeDeclaration(inner);
        } else if (ModelUtils.isMapSchema(s)) {
            MapSchema mapSchema = (MapSchema) s;
            Schema inner = (Schema) mapSchema.getAdditionalProperties();
            if (inner.getType() != null && inner.getType().equals("object")) {
                // Prevent `map[string]map[string]interface{}` when map value type is object
                return getSchemaType(s) + "[string]interface{}";
            } else {
                return getSchemaType(s) + "[string]" + getTypeDeclaration(inner);
            }
        }
        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String swaggerType = getSchemaType(s);
        if (typeMapping.containsKey(swaggerType)) {
            return typeMapping.get(swaggerType);
        }
        if (typeMapping.containsValue(swaggerType)) {
            return swaggerType;
        }
        if (languageSpecificPrimitives.contains(swaggerType)) {
            return swaggerType;
        }
        return toModelName(swaggerType);
    }

    private HashMap<String, CodegenModel> codegenModelMap = new HashMap<>();

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        objs = super.postProcessAllModels(objs);
        // Index all CodegenModels by model name.
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                codegenModelMap.put(cm.classname, cm);
            }
        }
        for (CodegenModel cm : codegenModelMap.values()) {
            markRecursiveProperties(cm, new CodegenProperty[0]);
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (property == null) return;
        // Override for custom swagger format: local-date-time
        // This is a datatype that contains a date and time, but no timezone
        if (property.dataType.equals("time.Time") && Boolean.TRUE.equals(property.isString) && property.isString) {
            property.isDateTime = true;
            property.vendorExtensions.put("x-local-date-time", "true");
        }
        if (property.dataType.equals("map[string]Object")) {
            property.dataType = "map[string]interface{}";
        }
        if (property.name.equals("Type")) {
            property.name = escapeReservedWord(property.name);
        }
    }

    private void markRecursiveProperties(CodegenModel cm, CodegenProperty[] lineage) {
        if (cm == null) return;
        String pad = StringUtils.leftPad("", lineage.length * 2, " ");
        for (CodegenProperty cp : cm.vars) {
            String lineageString = "0";
            for (CodegenProperty l : lineage) {
                lineageString += " > " + l.dataType;
                if (l.dataType.equalsIgnoreCase(cp.dataType) && !l.dataType.startsWith("*")) {
                    l.dataType = "*" + l.dataType;
                }
            }
            if ((Boolean.FALSE.equals(cp.isPrimitiveType) || !cp.isPrimitiveType) && codegenModelMap.containsKey(cp.dataType) && !Arrays.stream(lineage).anyMatch(l -> l.dataType.equalsIgnoreCase((cp.dataType)))) {
                ArrayList<CodegenProperty> lineageAL = new ArrayList<>(Arrays.asList(lineage));
                lineageAL.add(cp);
                CodegenProperty[] lineageArray = lineageAL.toArray(new CodegenProperty[0]);
                markRecursiveProperties(codegenModelMap.get(cp.dataType), lineageArray);
            }
        }
    }

    // override to post-process any parameters
    @SuppressWarnings("unused")
    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if (parameter.isBodyParam && parameter.paramName.equals("body") && parameter.dataType.equals("map[string]interface{}")) {
            parameter.dataType = "interface{}";
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        if (objs == null) {
            return objs;
        }
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        if (operations != null) {
            List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
            for (CodegenOperation operation : ops) {
                if (operation.httpMethod != null) {
                    operation.httpMethod = operation.httpMethod.toUpperCase(Locale.getDefault());
                }
            }
        }
        return objs;
    }
}

