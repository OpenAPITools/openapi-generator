package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.CodegenResponse;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.ArrayModel;
import io.swagger.models.Model;
import io.swagger.models.Response;
import io.swagger.models.parameters.Parameter;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.BooleanProperty;
import io.swagger.models.properties.DateProperty;
import io.swagger.models.properties.DateTimeProperty;
import io.swagger.models.properties.DoubleProperty;
import io.swagger.models.properties.FloatProperty;
import io.swagger.models.properties.IntegerProperty;
import io.swagger.models.properties.LongProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.properties.StringProperty;

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
        instantiationTypes.put("array", "List");

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
    public String toInstantiationType(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            String inner = getSwaggerType(ap.getItems());
            return instantiationTypes.get("array") + " " + inner;
        } else {
            return null;
        }
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

    @Override
    public CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions) {
        CodegenModel m = super.fromModel(name, model, allDefinitions);

        if (model instanceof ArrayModel) {
            ArrayModel am = (ArrayModel) model;
            ArrayProperty arrayProperty = new ArrayProperty(am.getItems());
            CodegenProperty codegenProperty = fromProperty(name, arrayProperty);
            m.vendorExtensions.putAll(codegenProperty.vendorExtensions);
        }

        return m;
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
                    parent.hasChildren = true;
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
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (cm.isEnum) {
                    this.addEncoderAndDecoder(cm.vendorExtensions, cm.classname, false);
                    cm.vendorExtensions.put(X_UNION_TYPE, cm.classname);
                } else if (cm.isAlias) {
                    this.addEncoderAndDecoder(cm.vendorExtensions, cm.dataType, true);
                }

                List<ElmImport> elmImports = new ArrayList<>();
                for (CodegenProperty property : cm.allVars) {
                    if (property.complexType != null) {
                        elmImports.add(createPropertyImport(property));
                    }
                }
                if (cm.isArrayModel) {
                    if (cm.arrayModelType != null) {
                        // add type imports
                        final ElmImport elmImport = new ElmImport();
                        final String modulePrefix = customPrimitives.contains(cm.arrayModelType) ? "" : "Data.";
                        elmImport.moduleName = modulePrefix + cm.arrayModelType;
                        elmImport.exposures = new TreeSet<>();
                        elmImport.exposures.add(cm.arrayModelType);
                        elmImport.exposures.add(camelize(cm.arrayModelType, true) + "Decoder");
                        elmImport.exposures.add(camelize(cm.arrayModelType, true) + "Encoder");
                        elmImport.hasExposures = true;
                        elmImports.add(elmImport);
                    }
                }
                if (cm.discriminator != null) {
                    for (CodegenModel child : cm.children) {
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
                        this.setDiscriminatorValue(child, cm.discriminator, this.getDiscriminatorValue(child));

                        // add all non-discriminator vars
                        int index = 0;
                        for (CodegenProperty property : cm.vars) {
                            if (!cm.discriminator.equals(property.baseName)) {
                                child.vars.add(index++, property);
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

        for (CodegenOperation op : ops) {
            String path = op.path;
            for (CodegenParameter param : op.pathParams) {
              final String var = param.isString ? param.paramName : "toString " + param.paramName;
                path = path.replace("{" + param.paramName + "}", "\" ++ " + var + " ++ \"");
            }
            op.path = ("\"" + path + "\"").replaceAll(" \\+\\+ \"\"", "");

            if (op.bodyParam != null) {
                final String encoder = (String) op.bodyParam.vendorExtensions.get(X_ENCODER);
                if (encoder != null) {
                    if (!dependencies.containsKey(op.bodyParam.dataType)) {
                        dependencies.put(op.bodyParam.dataType, new TreeSet<String>());
                    }
                    dependencies.get(op.bodyParam.dataType).add(encoder);
                }
            }
            for (CodegenResponse resp : op.responses) {
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
    public String toDefaultValue(Property p) {
        if (p instanceof StringProperty) {
            StringProperty sp = (StringProperty) p;
            if (sp.getDefault() != null) {
                return toOptionalValue("\"" + sp.getDefault().toString() + "\"");
            }
            return toOptionalValue(null);
        } else if (p instanceof BooleanProperty) {
            BooleanProperty bp = (BooleanProperty) p;
            if (bp.getDefault() != null) {
                return toOptionalValue(bp.getDefault() ? "True" : "False");
            }
            return toOptionalValue(null);
        } else if (p instanceof DateProperty) {
            return toOptionalValue(null);
        } else if (p instanceof DateTimeProperty) {
            return toOptionalValue(null);
        } else if (p instanceof DoubleProperty) {
            DoubleProperty dp = (DoubleProperty) p;
            if (dp.getDefault() != null) {
                return toOptionalValue(dp.getDefault().toString());
            }
            return toOptionalValue(null);
        } else if (p instanceof FloatProperty) {
            FloatProperty fp = (FloatProperty) p;
            if (fp.getDefault() != null) {
                return toOptionalValue(fp.getDefault().toString());
            }
            return toOptionalValue(null);
        } else if (p instanceof IntegerProperty) {
            IntegerProperty ip = (IntegerProperty) p;
            if (ip.getDefault() != null) {
                return toOptionalValue(ip.getDefault().toString());
            }
            return toOptionalValue(null);
        } else if (p instanceof LongProperty) {
            LongProperty lp = (LongProperty) p;
            if (lp.getDefault() != null) {
                return toOptionalValue(lp.getDefault().toString());
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
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
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
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getTypeDeclaration(inner);
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();
            return getTypeDeclaration(inner);
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public CodegenProperty fromProperty(String name, Property p) {
        final CodegenProperty property = super.fromProperty(name, p);

        final String dataType = property.isEnum ? property.baseName : property.datatype;
        addEncoderAndDecoder(property.vendorExtensions, dataType, property.isPrimitiveType && !property.isEnum);
        if (property.isEnum) {
            property.vendorExtensions.put(X_UNION_TYPE, property.datatypeWithEnum);
        }

        return property;
    }

    @Override
    public CodegenResponse fromResponse(String responseCode, Response resp) {
        final CodegenResponse response = super.fromResponse(responseCode, resp);
        if (response.dataType != null) {
            addEncoderAndDecoder(response.vendorExtensions, response.dataType, response.primitiveType);
        }
        return response;
    }

    @Override
    public CodegenParameter fromParameter(Parameter param, Set<String> imports) {
        final CodegenParameter parameter = super.fromParameter(param, imports);
        addEncoderAndDecoder(parameter.vendorExtensions, parameter.dataType, parameter.isPrimitiveType);
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
