package io.swagger.codegen.languages;

import io.swagger.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;

import java.io.File;
import java.util.*;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FileProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;

public abstract class AbstractTypeScriptClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final String X_DISCRIMINATOR_TYPE = "x-discriminator-value";
    private static final String UNDEFINED_VALUE = "undefined";

    protected String modelPropertyNaming= "camelCase";
    protected Boolean supportsES6 = true;
    protected HashSet<String> languageGenericTypes;

    public AbstractTypeScriptClientCodegen() {
        super();

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        supportsInheritance = true;
        setReservedWordsLowerCase(Arrays.asList(
                // local variable names used in API methods (endpoints)
                "varLocalPath", "queryParameters", "headerParams", "formParams", "useFormData", "varLocalDeferred",
                "requestOptions",
                // Typescript reserved words
                "abstract", "await", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "debugger", "default", "delete", "do", "double", "else", "enum", "export", "extends", "false", "final", "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int", "interface", "let", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "transient", "true", "try", "typeof", "var", "void", "volatile", "while", "with", "yield"));

        languageSpecificPrimitives = new HashSet<>(Arrays.asList(
                "string",
                "String",
                "boolean",
                "Boolean",
                "Double",
                "Integer",
                "Long",
                "Float",
                "Object",
                "Array",
                "Date",
                "number",
                "any",
                "File",
                "Error",
                "Map"
                ));

        languageGenericTypes = new HashSet<String>(Arrays.asList(
                "Array"
        ));

        instantiationTypes.put("array", "Array");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "any");
        typeMapping.put("integer", "number");
        typeMapping.put("Map", "any");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "Date");
        //TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("File", "any");
        typeMapping.put("Error", "Error");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue("camelCase"));
        cliOptions.add(new CliOption(CodegenConstants.SUPPORTS_ES6, CodegenConstants.SUPPORTS_ES6_DESC).defaultValue("false"));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }

        if (additionalProperties.containsKey(CodegenConstants.SUPPORTS_ES6)) {
            setSupportsES6(Boolean.valueOf(additionalProperties.get(CodegenConstants.SUPPORTS_ES6).toString()));
            additionalProperties.put("supportsES6", getSupportsES6());
        }
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String escapeReservedWord(String name) {
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name);

        if("_".equals(name)) {
            name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        name = getNameUsingModelPropertyNaming(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = camelize("model_" + name);
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            String modelName = camelize("model_" + name); // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        if (languageSpecificPrimitives.contains(name)) {
            String modelName = camelize("model_" + name);
            LOGGER.warn(name + " (model name matches existing language type) cannot be used as a model name. Renamed to " + modelName);
            return modelName;
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
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return "{ [key: string]: "+ getTypeDeclaration(inner) + "; }";
        } else if (p instanceof FileProperty) {
            return "any";
        }
        return super.getTypeDeclaration(p);
    }


    @Override
    protected String getParameterDataType(Parameter parameter, Property p) {
        // handle enums of various data types
        Property inner;
        if (p instanceof ArrayProperty) {
            ArrayProperty mp1 = (ArrayProperty) p;
            inner = mp1.getItems();
            return this.getSwaggerType(p) + "<" + this.getParameterDataType(parameter, inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            inner = mp.getAdditionalProperties();
            return "{ [key: string]: " + this.getParameterDataType(parameter, inner) + "; }";
        } else if (p instanceof StringProperty) {
            // Handle string enums
            StringProperty sp = (StringProperty) p;
            if (sp.getEnum() != null) {
                return enumValuesToEnumTypeUnion(sp.getEnum(), "string");
            }
        } else if (p instanceof IntegerProperty) {
            // Handle integer enums
            IntegerProperty sp = (IntegerProperty) p;
            if (sp.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(sp.getEnum()));
            }
        } else if (p instanceof LongProperty) {
            // Handle long enums
            LongProperty sp = (LongProperty) p;
            if (sp.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(sp.getEnum()));
            }
        } else if (p instanceof DoubleProperty) {
            // Handle double enums
            DoubleProperty sp = (DoubleProperty) p;
            if (sp.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(sp.getEnum()));
            }
        } else if (p instanceof FloatProperty) {
            // Handle float enums
            FloatProperty sp = (FloatProperty) p;
            if (sp.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(sp.getEnum()));
            }
        } else if (p instanceof DateProperty) {
            // Handle date enums
            DateProperty sp = (DateProperty) p;
            if (sp.getEnum() != null) {
                return enumValuesToEnumTypeUnion(sp.getEnum(), "string");
            }
        } else if (p instanceof DateTimeProperty) {
            // Handle datetime enums
            DateTimeProperty sp = (DateTimeProperty) p;
            if (sp.getEnum() != null) {
                return enumValuesToEnumTypeUnion(sp.getEnum(), "string");
            }
        }
        return this.getTypeDeclaration(p);
    }

    /**
     * Converts a list of strings to a literal union for representing enum values as a type.
     * Example output: 'available' | 'pending' | 'sold'
     *
     * @param values list of allowed enum values
     * @param dataType either "string" or "number"
     * @return
     */
    protected String enumValuesToEnumTypeUnion(List<String> values, String dataType) {
        StringBuilder b = new StringBuilder();
        boolean isFirst = true;
        for (String value: values) {
            if (!isFirst) {
                b.append(" | ");
            }
            b.append(toEnumValue(value.toString(), dataType));
            isFirst = false;
        }
        return b.toString();
    }

    /**
     * Converts a list of numbers to a literal union for representing enum values as a type.
     * Example output: 3 | 9 | 55
     *
     * @param values
     * @return
     */
    protected String numericEnumValuesToEnumTypeUnion(List<Number> values) {
        List<String> stringValues = new ArrayList<>();
        for (Number value: values) {
            stringValues.add(value.toString());
        }
        return enumValuesToEnumTypeUnion(stringValues, "number");
    }

    @Override
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            StringProperty sp = (StringProperty) p;
            if (sp.getDefault() != null) {
                return "'" + sp.getDefault() + "'";
            }
            return UNDEFINED_VALUE;
        } else if (p instanceof BooleanProperty) {
            return UNDEFINED_VALUE;
        } else if (p instanceof DateProperty) {
            return UNDEFINED_VALUE;
        } else if (p instanceof DateTimeProperty) {
            return UNDEFINED_VALUE;
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return dp.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (p instanceof FloatProperty) {
            FloatProperty fp = (FloatProperty) p;
            if (fp.getDefault() != null) {
                return fp.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (p instanceof IntegerProperty) {
            IntegerProperty ip = (IntegerProperty) p;
            if (ip.getDefault() != null) {
                return ip.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else if (p instanceof LongProperty) {
            LongProperty lp = (LongProperty) p;
            if (lp.getDefault() != null) {
                return lp.getDefault().toString();
            }
            return UNDEFINED_VALUE;
        } else {
            return UNDEFINED_VALUE;
        }
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type))
                return type;
        } else
            type = swaggerType;
        return toModelName(type);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        // append _ at the beginning, e.g. _return
        if (isReservedWord(operationId)) {
            return escapeReservedWord(camelize(sanitizeName(operationId), true));
        }

        return camelize(sanitizeName(operationId), true);
    }

    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) ||
            "PascalCase".equals(naming) || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" +
                                               naming + "'. Must be 'original', 'camelCase', " +
                                               "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:    return name;
            case camelCase:   return camelize(name, true);
            case PascalCase:  return camelize(name);
            case snake_case:  return underscore(name);
            default:          throw new IllegalArgumentException("Invalid model property naming '" +
                                                                 name + "'. Must be 'original', 'camelCase', " +
                                                                 "'PascalCase' or 'snake_case'");
        }

    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("number".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return camelize(getSymbolName(name));
        }

        // number
        if ("number".equals(datatype)) {
            String varName = "NUMBER_" + name;

            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String enumName = sanitizeName(name);
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        // camelize the enum variable name
        // ref: https://basarat.gitbooks.io/typescript/content/docs/enums.html
        enumName = camelize(enumName);

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name) + "Enum";

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            // name enum with model name, e.g. StatusEnum => Pet.StatusEnum
            for (CodegenProperty var : cm.vars) {
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + "." + var.enumName);
                }
            }
            if (cm.parent != null) {
                for (CodegenProperty var : cm.allVars) {
                    if (Boolean.TRUE.equals(var.isEnum)) {
                        var.datatypeWithEnum = var.datatypeWithEnum
                            .replace(var.enumName, cm.classname + "." + var.enumName);
                    }
                }
            }
        } 

        return objs;
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.discriminator != null && cm.children != null) {
                    for (CodegenModel child : cm.children) {
                        this.setDiscriminatorValue(child, cm.discriminator, this.getDiscriminatorValue(child));
                    }
                }
            }
        }
        return result;
    }

    public void setSupportsES6(Boolean value) {
        supportsES6 = value;
    }

    public Boolean getSupportsES6() {
        return supportsES6;
    }

    private void setDiscriminatorValue(CodegenModel model, String baseName, String value) {
        for (CodegenProperty prop : model.allVars) {
            if (prop.baseName.equals(baseName)) {
                prop.discriminatorValue = value;
            }
        }
        if (model.children != null) {
            final boolean newDiscriminator = model.discriminator != null;
            for (CodegenModel child : model.children) {
                this.setDiscriminatorValue(child, baseName, newDiscriminator ? value : this.getDiscriminatorValue(child));
            }
        }
    }

    private String getDiscriminatorValue(CodegenModel model) {
        return model.vendorExtensions.containsKey(X_DISCRIMINATOR_TYPE) ?
            (String) model.vendorExtensions.get(X_DISCRIMINATOR_TYPE) : model.classname;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ', " to avoid code injection
        return input.replace("\"", "").replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }
}
