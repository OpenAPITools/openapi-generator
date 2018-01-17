package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.languages.helpers.ExtensionHelper;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.BooleanSchema;
import io.swagger.v3.oas.models.media.DateSchema;
import io.swagger.v3.oas.models.media.DateTimeSchema;
import io.swagger.v3.oas.models.media.IntegerSchema;
import io.swagger.v3.oas.models.media.MapSchema;
import io.swagger.v3.oas.models.media.NumberSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;

import java.io.File;
import java.text.Collator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import static io.swagger.codegen.languages.helpers.ExtensionHelper.getBooleanValue;

public class ElmClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final String X_ENCODER = "x-encoder";
    private static final String X_DECODER = "x-decoder";
    private static final String X_DISCRIMINATOR_TYPE = "x-discriminator-value";
    private static final String X_UNION_TYPE = "x-union-type";

    private Set<String> customPrimitives = new HashSet<String>();

    protected String packageName = "swagger";
    protected String packageVersion = "1.0.0";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "elm";
    }

    public String getHelp() {
        return "Generates a Elm client library (beta).";
    }

    public ElmClientCodegen() {
        super();
        outputFolder = "generated-code/elm";
        modelTemplateFiles.put("model.mustache", ".elm");
        apiTemplateFiles.put("api.mustache", ".elm");
        templateDir = "elm";

        supportsInheritance = true;

        reservedWords = new HashSet<>(
          Arrays.asList(
            "if", "then", "else",
            "case", "of",
            "let", "in",
            "type",
            "module", "where",
            "import", "exposing",
            "as",
            "port")
        );

        defaultIncludes = new HashSet<>(
          Arrays.asList(
            "List")
        );

        languageSpecificPrimitives = new HashSet<>(
          Arrays.asList(
            "Bool",
            "Dict",
            "Float",
            "Int",
            "String")
        );

        customPrimitives = new HashSet<>(
          Arrays.asList(
            "Byte",
            "DateOnly",
            "DateTime")
        );

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Int");
        typeMapping.put("number", "Float");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("string", "String");
        typeMapping.put("array", "List");
        typeMapping.put("date", "DateOnly");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("password", "String");
        typeMapping.put("file", "String");
        typeMapping.put("ByteArray", "Byte");
        typeMapping.put("binary", "String");

        importMapping.clear();

        cliOptions.clear();

        supportingFiles.add(new SupportingFile("Byte.mustache", "src", "Byte.elm"));
        supportingFiles.add(new SupportingFile("DateOnly.mustache", "src", "DateOnly.elm"));
        supportingFiles.add(new SupportingFile("DateTime.mustache", "src", "DateTime.elm"));
        supportingFiles.add(new SupportingFile("Main.mustache", "src", "Main.elm"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("elm-package.mustache", "", "elm-package.json"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "");
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "Default";
        }
        return initialCaps(name);
    }

    @Override
    public String toModelName(String name) {
        return camelize(name);
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return toModelName(property.name);
    }

    @Override
    public String toVarName(String name) {
        final String varName = camelize(name, true);
        return isReservedWord(varName) ? escapeReservedWord(name) : varName;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        final String camelized = camelize(value.replace(" ", "_").replace("(", "_").replace(")", "")); // TODO FIXME escape properly
        if (!Character.isUpperCase(camelized.charAt(0))) {
            return "N" + camelized;
        }
        return camelized;
    }

    @Override
    public String escapeReservedWord(String name) {
        return name + "_";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/src/Request/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/src/Data/" + modelPackage().replace('.', File.separatorChar);
    }

    @SuppressWarnings({ "static-method", "unchecked" })
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = new HashMap<>();
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            String modelName = toModelName(entry.getKey());
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                allModels.put(modelName, cm);
            }
        }
        // Let parent know about all its children
        for (CodegenModel cm : allModels.values()) {
            CodegenModel parent = allModels.get(cm.parent);

            if (parent != null) {
                if (parent.children == null) {
                    parent.children = new ArrayList<>();
                    parent.getVendorExtensions().put(CodegenConstants.HAS_CHILDREN_EXT_NAME, Boolean.TRUE);
                }
                parent.children.add(cm);
                Collections.sort(parent.children, new Comparator<CodegenModel>() {
                    @Override
                    public int compare(CodegenModel cm1, CodegenModel cm2) {
                        return Collator.getInstance().compare(cm1.classname, cm2.classname);
                    }
                });
            }
        }
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel codegenModel = (CodegenModel) mo.get("model");

                if (getBooleanValue(codegenModel, CodegenConstants.IS_ENUM_EXT_NAME)) {
                    this.addEncoderAndDecoder(codegenModel.vendorExtensions, codegenModel.classname, false);
                    codegenModel.vendorExtensions.put(X_UNION_TYPE, codegenModel.classname);
                } else if (getBooleanValue(codegenModel, CodegenConstants.IS_ALIAS_EXT_NAME)) {
                    this.addEncoderAndDecoder(codegenModel.vendorExtensions, codegenModel.dataType, true);
                }

                List<ElmImport> elmImports = new ArrayList<>();
                for (CodegenProperty property : codegenModel.allVars) {
                    if (property.complexType != null) {
                        elmImports.add(createPropertyImport(property));
                    }
                }
                if (codegenModel.discriminator != null) {
                    for (CodegenModel child : codegenModel.children) {
                        // add child imports
                        final ElmImport elmImport = new ElmImport();
                        final String modulePrefix = customPrimitives.contains(child.classname) ? "" : "Data.";
                        elmImport.moduleName = modulePrefix + child.classname;
                        elmImport.exposures = new TreeSet<>();
                        elmImport.exposures.add(child.classname);
                        elmImport.exposures.add(child.classVarName + "Decoder");
                        elmImport.exposures.add(child.classVarName + "Encoder");
                        elmImport.hasExposures = true;
                        elmImports.add(elmImport);

                        // set discriminator value to all children (recursively)
                        if (codegenModel.discriminator != null) {
                            this.setDiscriminatorValue(child, codegenModel.discriminator.getPropertyName(), this.getDiscriminatorValue(child));
                            // add all non-discriminator vars
                            int index = 0;
                            for (CodegenProperty property : codegenModel.vars) {
                                if (!codegenModel.discriminator.getPropertyName().equals(property.baseName)) {
                                    child.vars.add(index++, property);
                                }
                            }
                        }
                    }
                }
                inner.put("elmImports", elmImports);
            }
        }
        return objs;
    }

    private void setDiscriminatorValue(CodegenModel model, String baseName, String value) {
        for (CodegenProperty prop : model.vars) {
            if (prop.baseName.equals(baseName)) {
                prop.discriminatorValue = value;
            }
        }
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

    private ElmImport createPropertyImport(final CodegenProperty property) {
        final ElmImport elmImport = new ElmImport();
        final String modulePrefix = customPrimitives.contains(property.complexType) ? "" : "Data.";
        elmImport.moduleName = modulePrefix + property.complexType;
        elmImport.exposures = new TreeSet<>();
        elmImport.exposures.add(property.complexType);
        if (property.vendorExtensions.containsKey(X_DECODER)) {
            elmImport.exposures.add((String) property.vendorExtensions.get(X_DECODER));
        }
        if (property.vendorExtensions.containsKey(X_ENCODER)) {
            elmImport.exposures.add((String) property.vendorExtensions.get(X_ENCODER));
        }
        elmImport.hasExposures = true;
        return elmImport;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    @Override
    @SuppressWarnings({ "static-method", "unchecked" })
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");
        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");

        Map<String, Set<String>> dependencies = new HashMap<>();

        for (CodegenOperation codegenOperation : ops) {
            String path = codegenOperation.path;
            for (CodegenParameter param : codegenOperation.pathParams) {
              final String var = getBooleanValue(param, CodegenConstants.IS_STRING_EXT_NAME) ? param.paramName : "toString " + param.paramName;
                path = path.replace("{" + param.paramName + "}", "\" ++ " + var + " ++ \"");
            }
            codegenOperation.path = ("\"" + path + "\"").replaceAll(" \\+\\+ \"\"", "");

            if (codegenOperation.bodyParam != null) {
                final String encoder = (String) codegenOperation.bodyParam.vendorExtensions.get(X_ENCODER);
                if (encoder != null) {
                    if (!dependencies.containsKey(codegenOperation.bodyParam.dataType)) {
                        dependencies.put(codegenOperation.bodyParam.dataType, new TreeSet<String>());
                    }
                    dependencies.get(codegenOperation.bodyParam.dataType).add(encoder);
                }
            }
            for (CodegenResponse resp : codegenOperation.responses) {
                final String decoder = (String) resp.vendorExtensions.get(X_DECODER);
                if (decoder != null) {
                    if (!dependencies.containsKey(resp.dataType)) {
                        dependencies.put(resp.dataType, new TreeSet<String>());
                    }
                    dependencies.get(resp.dataType).add(decoder);
                }
            }
        }

        List<ElmImport> elmImports = new ArrayList<>();
        for (Map.Entry<String, Set<String>> entry : dependencies.entrySet()) {
            final ElmImport elmImport = new ElmImport();
            final String key = entry.getKey();
            elmImport.moduleName = "Data." + key;
            elmImport.exposures = entry.getValue();
            elmImport.exposures.add(key);
            elmImport.hasExposures = true;
            elmImports.add(elmImport);
        }
        operations.put("elmImports", elmImports);

        return operations;
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema instanceof StringSchema) {
            StringSchema stringSchema = (StringSchema) schema;
            if (stringSchema.getDefault() != null) {
                return toOptionalValue("\"" + stringSchema.getDefault().toString() + "\"");
            }
            return toOptionalValue(null);
        } else if (schema instanceof BooleanSchema) {
            BooleanSchema booleanSchema = (BooleanSchema) schema;
            if (booleanSchema.getDefault() != null) {
                return toOptionalValue(booleanSchema.getDefault() != null && Boolean.TRUE.equals(booleanSchema.getDefault()) ? "True" : "False");
            }
            return toOptionalValue(null);
        } else if (schema instanceof DateSchema) {
            return toOptionalValue(null);
        } else if (schema instanceof DateTimeSchema) {
            return toOptionalValue(null);
        } else if (schema instanceof NumberSchema) {
            NumberSchema numberSchema = (NumberSchema) schema;
            if (numberSchema.getDefault() != null) {
                return toOptionalValue(numberSchema.getDefault().toString());
            }
            return toOptionalValue(null);
        } else if (schema instanceof IntegerSchema) {
            IntegerSchema integerSchema = (IntegerSchema) schema;
            if (integerSchema.getDefault() != null) {
                return toOptionalValue(integerSchema.getDefault().toString());
            }
            return toOptionalValue(null);
        } else {
            return toOptionalValue(null);
        }
    }
    
    private String toOptionalValue(String value) {
        if (value == null) {
            return "Nothing";
        }
        return "(Just " + value + ")";
    }

    @Override
    public String getSchemaType(Schema schema) {
        String swaggerType = super.getSchemaType(schema);
        String type;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else
            type = swaggerType;
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Schema schema) {
        if (schema instanceof ArraySchema) {
            ArraySchema arraySchema = (ArraySchema) schema;
            Schema inner = arraySchema.getItems();
            return getTypeDeclaration(inner);
        } else if (schema instanceof MapSchema && hasSchemaProperties(schema)) {
            MapSchema mapSchema = (MapSchema) schema;
            Schema inner = (Schema) mapSchema.getAdditionalProperties();
            return getTypeDeclaration(inner);
        }
        return super.getTypeDeclaration(schema);
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema schema) {
        final CodegenProperty property = super.fromProperty(name, schema);
        final boolean isEnum = getBooleanValue(property, CodegenConstants.IS_ENUM_EXT_NAME);
        final boolean isPrimitiveType = getBooleanValue(property, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME);
        final String dataType = isEnum ? property.baseName : property.datatype;

        addEncoderAndDecoder(property.vendorExtensions, dataType, isPrimitiveType && !isEnum);
        if (isEnum) {
            property.vendorExtensions.put(X_UNION_TYPE, property.datatypeWithEnum);
        }

        return property;
    }

    @Override
    public CodegenResponse fromResponse(String responseCode, ApiResponse resp) {
        final CodegenResponse response = super.fromResponse(responseCode, resp);
        if (response.dataType != null) {
            addEncoderAndDecoder(response.vendorExtensions, response.dataType, getBooleanValue(response, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME));
        }
        return response;
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        final CodegenParameter parameter = super.fromParameter(param, imports);
        final boolean isPrimitiveType = getBooleanValue(parameter, CodegenConstants.IS_PRIMITIVE_TYPE_EXT_NAME);
        addEncoderAndDecoder(parameter.vendorExtensions, parameter.dataType, isPrimitiveType);
        return parameter;
    }

    private void addEncoderAndDecoder(Map<String, Object> vendorExtensions, String dataType, Boolean isPrimitiveType) {
        final String baseName = camelize(dataType, true);
        String encoderName;
        String decoderName;
        if (isPrimitiveType) {
            encoderName = "Encode." + baseName;
            decoderName = "Decode." + baseName;
        } else {
            encoderName = baseName + "Encoder";
            decoderName = baseName + "Decoder";
        }
        if (!vendorExtensions.containsKey(X_ENCODER)) {
            vendorExtensions.put(X_ENCODER, encoderName);
        }
        if (!vendorExtensions.containsKey(X_DECODER)) {
            vendorExtensions.put(X_DECODER, decoderName);
        }
    }

    private static class ElmImport {
        public String moduleName;
        public String as;
        public Set<String> exposures;
        public Boolean hasExposures;
    }
}