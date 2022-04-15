package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class GoCliClientCodegen extends PureCloudGoClientCodegen {
    protected Logger LOGGER = LoggerFactory.getLogger(GoCliClientCodegen.class);

    public GoCliClientCodegen() {
        super();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        outputFolder = "generated-code/go";
        embeddedTemplateDir = templateDir = "go";
    }

    @Override
    public String getName() {
        return "clisdkclient";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        // Wipe out all the templates so we can start with a clean slate
        this.apiTemplateFiles.clear();
        this.apiDocTemplateFiles.clear();
        this.apiTestTemplateFiles.clear();
        this.modelTemplateFiles.clear();
        this.modelDocTemplateFiles.clear();
        this.modelTestTemplateFiles.clear();
        //this.operationTemplateFiles.clear();
        this.supportingFiles.clear();

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

        additionalProperties.put("addImports", "{{addit.addImports}}");
        additionalProperties.put("addCommands", "{{addit.addCommands}}");

        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.md", "", "README.md"));
        supportingFiles.add(new SupportingFile("Makefile.mustache", "", "Makefile"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("restclient.mustache", "/gc/restclient", "restclient.go"));
        supportingFiles.add(new SupportingFile("root.mustache", "/gc/cmd", "root.go"));
        apiTemplateFiles.put("api.mustache", ".go");
        modelTemplateFiles.put("model.mustache", ".go");
    }

    public String modelFileFolder() {
        return (outputFolder + "/gc/models").replace('/', File.separatorChar);
    }

    public String apiFileFolder() {
        return (outputFolder + "/gc/cmd").replace('/', File.separatorChar);
    }

    @Override
    public String toApiFilename(String name) {
        name = name.toLowerCase(Locale.getDefault());
        name = name + File.separatorChar + name;

        name = name.replaceAll("_test$", "_testfile");
        name = name.replaceAll("_test/", "_testfile/");
        name = name.replaceAll("-", "");

        return name;
    }

    @Override
    public String toOperationId(String operationId) {
        return operationId;
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
        return operation.getOperationId();
    }

    /**
     * Remove characters not suitable for variable or method name from the input and camelize it
     *
     * @param name string to be camelize
     * @return camelized string
     */
    @SuppressWarnings("static-method")
    public String removeNonNameElementToCamelCase(String name) {
        // Change to no-op
        return name;
    }

    @Override
    /**
     * Add operation to group
     *
     * @param tag name of the tag
     * @param resourcePath path of the resource
     * @param operation Swagger Operation object
     * @param co Codegen Operation object
     * @param operations map of Codegen operations
     */
    @SuppressWarnings("static-method")
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation
            co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(tag);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(tag, opList);
        }

        co.operationIdLowerCase = co.operationId.toLowerCase(Locale.getDefault());
        opList.add(co);
        co.baseName = tag;
    }

    @Override
    public String toApiVarName(String name) {
        name = name.toLowerCase(Locale.getDefault());
        name = name.replaceAll("_test$", "_testfile");
        name = name.replaceAll("_test/", "_testfile/");

        return name;
    }

    private String mapWithDefaultValues(String datatype) {
        switch (datatype) {
            case "map[string]string":
                return datatype + "{\"\": \"\"}";
            case "map[string]bool":
                return datatype + "{\"\": true}";
            case "map[string]interface{}":
                return datatype + "{\"\": Interface{}}";
            case "map[string]int":
                return datatype + "{\"\": 0}";
            case "map[string]float64":
            case "map[string]float32":
                return datatype + "{\"\": 0.0}";
            default:
                return datatype + "{\"\": {}}";
        }
    }

    private String listWithDefaultValue(String datatype) {
        switch (datatype) {
            case "[]string":
                return datatype + "{\"\"}";
            case "[]int":
                return datatype + "{0}";
            case "[]float64":
            case "[]float32":
                return datatype + "{0.0}";
            case "[]interface{}":
                return datatype + "{}";
            default:
                return datatype + "{{}}";
        }
    }

    // This vendor extension is used as a template var to define json objects as lists or maps with default values of their type
    // so that they're printed to the cli correctly and not as the default null.
    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        String customPropertyName = "x-dataTypeWithDefaults";
        String customValue;
        if (property.dataType.equals("interface{}"))
            customValue = "Interface{}";
        else if (property.isMap)
            customValue = mapWithDefaultValues(property.dataType);
        else if (property.isArray)
            customValue = listWithDefaultValue(property.dataType);
        else
            return;
        property.vendorExtensions.put(customPropertyName, customValue);
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        if (parameter.description != null)
            parameter.description = processDescription(parameter.description);
        if (!parameter.isBodyParam)
            return;
        if (parameter.baseType == null || parameter.baseType.equals(""))
            return;

        if (parameter.baseType.contains("interface{}")) {
            parameter.vendorExtensions.put("x-baseType", "Interface");
        } else {
            // Set x-baseType as baseType but with the naming convention of the model template inside clisdkclient (Capitalfirstletteronly)
            StringBuilder sb = new StringBuilder(parameter.baseType.toLowerCase(Locale.getDefault()));
            char firstChar = parameter.baseType.charAt(0);
            sb.setCharAt(0, Character.toUpperCase(firstChar));
            parameter.vendorExtensions.put("x-baseType", sb.toString());
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            if (operation.summary != null) {
                operation.summary = processDescription(operation.summary);
            }
        }
        return super.postProcessOperationsWithModels(objs, allModels);
    }

    private String processDescription(String description) {
        return description
                .replace("\\\"", "")
                .replace("&", "and")
                .replace("'", "`");
    }

    @Override
    public String sanitizeTag(String tag) {
        // remove spaces and make strong case
        String[] parts = tag.split(" ");
        StringBuilder buf = new StringBuilder();
        for (String part : parts) {
            if (StringUtils.isNotEmpty(part)) {
                buf.append(StringUtils.capitalize(part));
            }
        }
        return buf.toString().replaceAll("[^a-zA-Z0-9_ ]", "");
    }
}