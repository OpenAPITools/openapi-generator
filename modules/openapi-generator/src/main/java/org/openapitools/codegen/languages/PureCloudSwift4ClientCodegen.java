package org.openapitools.codegen.languages;

//import io.swagger.models.Operation;
import io.swagger.v3.oas.models.Operation;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class PureCloudSwift4ClientCodegen extends Swift4Codegen {
    private static String OPERATION_ID_PROPERTY_NAME = "x-purecloud-method-name";

    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected Logger LOGGER = LoggerFactory.getLogger(PureCloudSwift4ClientCodegen.class);

    public PureCloudSwift4ClientCodegen() {
        super();

        // Use default templates
        embeddedTemplateDir = templateDir = "Swift";

        // Additional templates
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        // Custom mappings and overrides for swagger type -> swift type
        typeMapping.put("object", "JSON");
        typeMapping.put("LocalDateTime", "String");
        typeMapping.put("URI", "String");
        typeMapping.put("uri", "String");
        typeMapping.put("Dictionary", "String:JSON");
        typeMapping.put("dictionary", "String:JSON");
        typeMapping.put("[dictionary]", "String:JSON");
        typeMapping.put("[Dictionary]", "String:JSON");
        typeMapping.put("array", "String:JSON");

        // Documentation
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // New stuff
        supportingFiles.add(new SupportingFile("UrlSessionImplementations.mustache", sourceFolder, "UrlSessionImplementations.swift"));

        setRemoveEnumValuePrefix(false);
    }

    @Override
    public String getName() { return "purecloudios"; }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String type = p.baseType;
        if (type == null || type.equals(""))
            type = p.dataType;
        if (type != null && !type.equals("")) {
            if (p.defaultValue != null && !p.defaultValue.equals((""))) {
                p.example = p.defaultValue;
                if (type.toLowerCase(Locale.getDefault()).equals("string")) {
                    p.example = "\"" + p.example + "\"";
                }
            } else {
                switch (type.toLowerCase(Locale.getDefault())) {
                    case "character":
                    case "string": {
                        p.example = "\"\"";
                        break;
                    }
                    case "int32":
                    case "int64":
                    case "int": {
                        p.example = "0";
                        break;
                    }
                    case "double":
                    case "float": {
                        p.example = "0";
                        break;
                    }
                    case "bool": {
                        p.example = "true";
                        break;
                    }
                    case "any": {
                        p.example = "[Any]";
                        break;
                    }
                    case "anyobject": {
                        p.example = "[AnyObject]";
                        break;
                    }
                    default: {
                        p.example = "new " + type + "(...)";
                    }
                }

                if (p.isArray) {
                    p.example = "[" + p.example + "]";
                }
            }
        }
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

    @Override
    public String toEnumVarName(String name, String datatype) {
        String enumVarName = super.toEnumVarName(name, datatype);
        enumVarName = enumVarName.replaceAll("[\\W]", "_");
        return enumVarName.replace("*", "Wildcard");
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        if (parameter.isEnum) {
            if (parameter.datatypeWithEnum.startsWith("["))
                parameter.enumName = parameter.datatypeWithEnum.substring(1, parameter.datatypeWithEnum.length() - 1);
            else
                parameter.enumName = parameter.datatypeWithEnum;

            if (parameter.allowableValues == null || !parameter.allowableValues.containsKey("values")) return;

            ArrayList values = (ArrayList)parameter.allowableValues.get("values");
            List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
            for (int i = 0; i < values.size(); i++) {
                Map<String, String> enumVar = new HashMap<String, String>();
                String s = values.get(i).toString();
                enumVar.put("value", s);

                // Replace non-alphanumeric chars in name with underscore
                s = toEnumVarName(s, "string");

                enumVar.put("name", s);
                enumVars.add(enumVar);
            }
            parameter.allowableValues.clear();
            parameter.allowableValues.put("enumVars", enumVars);
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        if (property.isContainer &&
                property.isArray &&
                property.containerType != null && property.containerType.equals("array") &&
                property.items != null && property.items.isEnum) {

            property.isEnum = true;

            if (property.datatypeWithEnum.startsWith("["))
                property.enumName = property.datatypeWithEnum.substring(1, property.datatypeWithEnum.length() - 1);
            else
                property.enumName = property.datatypeWithEnum;

            property.allowableValues = property.items.allowableValues;
        }
        property.dataType = property.dataType.replace("[Dictionary]", "[[String:JSON]]");
        property.datatypeWithEnum = property.datatypeWithEnum.replace("[Dictionary]", "[[String:JSON]]");

        if (property.baseType.equals("StringJSON") && !(property.complexType == null)) {
            if (!property.datatypeWithEnum.contains("[[String:JSON]]") || !property.dataType.contains("[[String:JSON]]")) {
                boolean startsWithThing = property.dataType.startsWith("[") || property.datatypeWithEnum.startsWith("[");
                property.dataType = toModelName(property.dataType);
                property.datatypeWithEnum = toModelName(property.datatypeWithEnum);

                if (startsWithThing && !property.dataType.startsWith("[") || !property.datatypeWithEnum.startsWith("[")) {
                    property.dataType = "[" + property.dataType + "]";
                    property.datatypeWithEnum = "[" + property.datatypeWithEnum + "]";
                }
            }
        }
    }
}
