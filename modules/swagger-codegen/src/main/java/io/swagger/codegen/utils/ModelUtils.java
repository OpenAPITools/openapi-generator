package io.swagger.codegen.utils;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.oas.models.Operation;
import io.swagger.oas.models.PathItem;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static io.swagger.codegen.CodegenConstants.IS_ENUM_EXT_NAME;
import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class ModelUtils {
    /**
     * Searches for the model by name in the map of models and returns it
     *
     * @param name Name of the model
     * @param models Map of models
     * @return model
     */
    public static CodegenModel getModelByName(final String name, final Map<String, Object> models) {
        final Object data = models.get(name);
        if (data instanceof Map) {
            final Map<?, ?> dataMap = (Map<?, ?>) data;
            final Object dataModels = dataMap.get("models");
            if (dataModels instanceof List) {
                final List<?> dataModelsList = (List<?>) dataModels;
                for (final Object entry : dataModelsList) {
                    if (entry instanceof Map) {
                        final Map<?, ?> entryMap = (Map<?, ?>) entry;
                        final Object model = entryMap.get("model");
                        if (model instanceof CodegenModel) {
                            return (CodegenModel) model;
                        }
                    }
                }
            }
        }
        return null;
    }

    public static Operation[] createOperationArray (PathItem pathItem) {
        return new Operation[]{
                pathItem.getGet(),
                pathItem.getPost(),
                pathItem.getDelete(),
                pathItem.getHead(),
                pathItem.getPut(),
                pathItem.getPatch(),
                pathItem.getOptions()
        };
    }

    public static void processCodegenModels(Map<String, CodegenModel> allModels) {
        // Fix up all parent and interface CodegenModel references.
        for (CodegenModel codegenModel : allModels.values()) {
            if (codegenModel.getParent() != null) {
                codegenModel.setParentModel(allModels.get(codegenModel.getParent()));
            }
            if (codegenModel.getInterfaces() == null || codegenModel.getInterfaces().isEmpty()) {
                continue;
            }
            codegenModel.setInterfaceModels(new ArrayList<CodegenModel>(codegenModel.getInterfaces().size()));
            for (String intf : codegenModel.getInterfaces()) {
                CodegenModel intfModel = allModels.get(intf);
                if (intfModel != null) {
                    codegenModel.getInterfaceModels().add(intfModel);
                }
            }
        }
        // Let parent know about all its children
        for (String name : allModels.keySet()) {
            CodegenModel codegenModel = allModels.get(name);
            CodegenModel parent = allModels.get(codegenModel.getParent());
            // if a discriminator exists on the parent, don't add this child to the inheritance heirarchy
            // TODO Determine what to do if the parent discriminator name == the grandparent discriminator name
            while (parent != null) {
                if (parent.getChildren() == null) {
                    parent.setChildren(new ArrayList<CodegenModel>());
                }
                parent.getChildren().add(codegenModel);
                if (parent.getDiscriminator() == null) {
                    parent = allModels.get(parent.parent);
                } else {
                    parent = null;
                }
            }
        }
    }

    public static void processModelEnums(Map<String, Object> objs) {
        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");

            // for enum model
            boolean isEnum = getBooleanValue(cm.getVendorExtensions(), IS_ENUM_EXT_NAME);
            if (Boolean.TRUE.equals(isEnum) && cm.allowableValues != null) {
                Map<String, Object> allowableValues = cm.allowableValues;
                List<Object> values = (List<Object>) allowableValues.get("values");
                List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
                String commonPrefix = findCommonPrefixOfVars(values);
                int truncateIdx = commonPrefix.length();
                for (Object value : values) {
                    Map<String, String> enumVar = new HashMap<String, String>();
                    String enumName;
                    if (truncateIdx == 0) {
                        enumName = value.toString();
                    } else {
                        enumName = value.toString().substring(truncateIdx);
                        if ("".equals(enumName)) {
                            enumName = value.toString();
                        }
                    }
                    enumVar.put("name", toEnumVarName(enumName));
                    enumVar.put("value", toEnumValue(value.toString(), cm.dataType));
                    enumVars.add(enumVar);
                }
                cm.allowableValues.put("enumVars", enumVars);
            }

            // update codegen property enum with proper naming convention
            // and handling of numbers, special characters
            for (CodegenProperty var : cm.vars) {
                updateCodegenPropertyEnum(var);
            }
        }
    }

    /**
     * Returns the common prefix of variables for enum naming
     *
     * @param vars List of variable names
     * @return the common prefix for naming
     */
    public static String findCommonPrefixOfVars(List<Object> vars) {
        try {
            String[] listStr = vars.toArray(new String[vars.size()]);
            String prefix = StringUtils.getCommonPrefix(listStr);
            // exclude trailing characters that should be part of a valid variable
            // e.g. ["status-on", "status-off"] => "status-" (not "status-o")
            return prefix.replaceAll("[a-zA-Z0-9]+\\z", "");
        } catch (ArrayStoreException e) {
            return "";
        }
    }

    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    public static void updateCodegenPropertyEnum(CodegenProperty var) {
        Map<String, Object> allowableValues = var.allowableValues;

        // handle ArrayProperty
        if (var.items != null) {
            allowableValues = var.items.allowableValues;
        }

        if (allowableValues == null) {
            return;
        }

        List<Object> values = (List<Object>) allowableValues.get("values");
        if (values == null) {
            return;
        }

        // put "enumVars" map into `allowableValues", including `name` and `value`
        List<Map<String, String>> enumVars = new ArrayList<Map<String, String>>();
        String commonPrefix = findCommonPrefixOfVars(values);
        int truncateIdx = commonPrefix.length();
        for (Object value : values) {
            Map<String, String> enumVar = new HashMap<String, String>();
            String enumName;
            if (truncateIdx == 0) {
                enumName = value.toString();
            } else {
                enumName = value.toString().substring(truncateIdx);
                if ("".equals(enumName)) {
                    enumName = value.toString();
                }
            }
            enumVar.put("name", toEnumVarName(enumName));
            enumVar.put("value", toEnumValue(value.toString(), var.datatype));
            enumVars.add(enumVar);
        }
        allowableValues.put("enumVars", enumVars);

        // handle default value for enum, e.g. available => StatusEnum.AVAILABLE
        if (var.defaultValue != null) {
            String enumName = null;
            for (Map<String, String> enumVar : enumVars) {
                if (toEnumValue(var.defaultValue, var.datatype).equals(enumVar.get("value"))) {
                    enumName = enumVar.get("name");
                    break;
                }
            }
            if (enumName != null) {
                var.defaultValue = String.format("%s.%s", enumName, var.datatypeWithEnum);
            }
        }
    }

    public static String toEnumVarName(String value) {
        if (value.length() == 0) {
            return "EMPTY";
        }
        String var = value.replaceAll("\\W+", "_").toUpperCase();
        if (var.matches("\\d.*")) {
            return "_" + var;
        } else {
            return var;
        }
    }

    private static String toEnumValue(String value, String datatype) {
        if ("number".equalsIgnoreCase(datatype)) {
            return value;
        } else {
            value = StringEscapeUtils.unescapeJava(
                    StringEscapeUtils.escapeJava(value)
                            .replace("\\/", "/"))
                    .replaceAll("[\\t\\n\\r]"," ")
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"");
            return String.format("\"%s\"", value);
        }
    }
}
