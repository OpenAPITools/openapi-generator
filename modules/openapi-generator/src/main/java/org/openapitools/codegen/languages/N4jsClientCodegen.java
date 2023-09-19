/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import static org.openapitools.codegen.CodegenConstants.API_NAME_PREFIX;
import static org.openapitools.codegen.CodegenConstants.API_NAME_PREFIX_DESC;
import static org.openapitools.codegen.CodegenConstants.API_PACKAGE;
import static org.openapitools.codegen.CodegenConstants.API_PACKAGE_DESC;
import static org.openapitools.codegen.CodegenConstants.MODEL_PACKAGE;
import static org.openapitools.codegen.CodegenConstants.MODEL_PACKAGE_DESC;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenSecurity;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.IJsonSchemaValidationProperties;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;

public class N4jsClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String CHECK_REQUIRED_PARAMS_NOT_NULL = "checkRequiredParamsNotNull";
    public static final String CHECK_SUPERFLUOUS_BODY_PROPS = "checkSuperfluousBodyProps";
    public static final String GENERATE_DEFAULT_API_EXECUTER = "generateDefaultApiExecuter";

    final Logger LOGGER = LoggerFactory.getLogger(N4jsClientCodegen.class);

    final Set<String> forbiddenChars = new HashSet<>();

    private boolean checkRequiredBodyPropsNotNull = true;
    private boolean checkSuperfluousBodyProps = true;
    private boolean generateDefaultApiExecuter = true;

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "n4js";
    }

    public String getHelp() {
        return "Generates a n4js client.";
    }

    public N4jsClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        specialCharReplacements.clear();

        outputFolder = "generated-code" + File.separator + "n4js";
        modelTemplateFiles.put("model.mustache", ".n4jsd");
        apiTemplateFiles.put("api.mustache", ".n4js");
        embeddedTemplateDir = templateDir = "n4js";
        apiPackage = "";
        modelPackage = "";

        typeMapping = new HashMap<String, String>();
        typeMapping.put("Set", "Set");
        typeMapping.put("set", "Set");
        typeMapping.put("Array", "Array");
        typeMapping.put("array", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("char", "string");
        typeMapping.put("float", "number");
        typeMapping.put("long", "int");
        typeMapping.put("short", "int");
        typeMapping.put("int", "int");
        typeMapping.put("integer", "int");
        typeMapping.put("number", "number");
        typeMapping.put("double", "number");
        typeMapping.put("object", "object");
        typeMapping.put("Map", "any");
        typeMapping.put("map", "any");
        typeMapping.put("date", "string");
        typeMapping.put("DateTime", "string");
        typeMapping.put("binary", "any");
        typeMapping.put("File", "any");
        typeMapping.put("file", "any");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("Error", "Error");
        typeMapping.put("AnyType", "any");

        importMapping.clear(); // not used

        supportsInheritance = true;
        supportsMultipleInheritance = false;

        reservedWords.addAll(Arrays.asList(
                // local variable names used in API methods (endpoints)
                "varLocalPath", "queryParameters", "headerParams", "formParams", "useFormData", "varLocalDeferred",
                "requestOptions",
                // N4JS reserved words
                "abstract", "await", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
                "debugger", "default", "delete", "do", "double", "else", "enum", "export", "extends", "false", "final",
                "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int",
                "interface", "let", "long", "native", "new", "null", "package", "private", "protected", "public",
                "return", "short", "static", "super", "switch", "synchronized", "this", "throw", "transient", "true",
                "try", "typeof", "var", "void", "volatile", "while", "with", "yield"));

        languageSpecificPrimitives = new HashSet<>(Arrays.asList("string", "String", "boolean", "number", "int",
                "Object", "object", "Array", "any", "any+", "Error"));

        defaultIncludes.add("~Object+");
        defaultIncludes.add("Object+");

        forbiddenChars.add("@");

        cliOptions.clear();
        cliOptions.add(new CliOption(API_PACKAGE, API_PACKAGE_DESC));
        cliOptions.add(new CliOption(MODEL_PACKAGE, MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(API_NAME_PREFIX, API_NAME_PREFIX_DESC));
        cliOptions.add(new CliOption(CHECK_REQUIRED_PARAMS_NOT_NULL,
                "Iff true null-checks are performed for required parameters."));
        cliOptions.add(new CliOption(CHECK_SUPERFLUOUS_BODY_PROPS,
                "Iff true a new copy of the given body object is transmitted. This copy only contains those properties defined in its model specification."));
        cliOptions.add(new CliOption(GENERATE_DEFAULT_API_EXECUTER,
                "Iff true a default implementation of the api executer interface is generated."));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // disable since otherwise Modules/Classes are not generated iff used as
        // parameters only
        GlobalSettings.setProperty("skipFormModel", "false");

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiHelper.mustache", apiPackage, "ApiHelper.n4js"));

        checkRequiredBodyPropsNotNull = processBooleanOpt(CHECK_REQUIRED_PARAMS_NOT_NULL,
                checkRequiredBodyPropsNotNull);
        checkSuperfluousBodyProps = processBooleanOpt(CHECK_SUPERFLUOUS_BODY_PROPS, checkSuperfluousBodyProps);
        generateDefaultApiExecuter = processBooleanOpt(GENERATE_DEFAULT_API_EXECUTER, generateDefaultApiExecuter);

        if (additionalProperties.get(API_PACKAGE) instanceof String) {
            apiPackage = additionalProperties.get(API_PACKAGE).toString();
        } else {
            additionalProperties.put(API_PACKAGE, apiPackage);
        }

        if (additionalProperties.get(MODEL_PACKAGE) instanceof String) {
            modelPackage = additionalProperties.get(MODEL_PACKAGE).toString();
        } else {
            additionalProperties.put(MODEL_PACKAGE, modelPackage);
        }

        if (additionalProperties.get(API_NAME_PREFIX) instanceof String) {
            apiNamePrefix = additionalProperties.get(API_NAME_PREFIX).toString();
        } else {
            additionalProperties.put(API_NAME_PREFIX, apiNamePrefix);
        }
    }

    private boolean processBooleanOpt(String OPT, boolean defaultValue) {
        boolean passedValue = defaultValue;
        if (additionalProperties.containsKey(OPT)) {
            Object value = additionalProperties.get(OPT);
            if (value instanceof Boolean) {
                passedValue = (Boolean) value;
            } else {
                try {
                    passedValue = Boolean.parseBoolean(value.toString());
                } catch (Exception e) {
                    // ignore
                }
            }
        }
        additionalProperties.put(OPT, passedValue);
        return defaultValue;
    }

    @Override
    public String toModelFilename(String name) {
        String modelFilename = super.toModelFilename(name);
        if (typeMapping.containsKey(modelFilename) || defaultIncludes.contains(modelFilename)) {
            return modelFilename;
        }
        return modelFilename;
    }

    public boolean checkRequiredBodyPropsNotNull() {
        return checkRequiredBodyPropsNotNull;
    }

    public boolean checkSuperfluousBodyProps() {
        return checkSuperfluousBodyProps;
    }

    public boolean generateDefaultApiExecuter() {
        return generateDefaultApiExecuter;
    }

    @Override
    public boolean getUseInlineModelResolver() {
        return false;
    }

    @Override
    public void setOpenAPI(OpenAPI openAPI) {
        super.setOpenAPI(openAPI);
        typeAliases.put("object", "~Object+");
    }

    @Override
    protected boolean isReservedWord(String word) {
        // case sensitive matching
        return reservedWords.contains(word);
    }

    @Override
    public String toAnyOfName(List<String> names, Schema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getAnyOf());
        return String.join(" | ", types);
    }

    @Override
    public String toOneOfName(List<String> names, Schema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getOneOf());
        return String.join(" | ", types);
    }

    @Override
    public String toAllOfName(List<String> names, Schema composedSchema) {
        List<String> types = getTypesFromSchemas(composedSchema.getAllOf());
        return String.join(" & ", types);
    }

    /**
     * Extracts the list of type names from a list of schemas. Excludes `AnyType` if
     * there are other valid types extracted.
     *
     * @param schemas list of schemas
     * @return list of types
     */
    @SuppressWarnings("rawtypes")
    protected List<String> getTypesFromSchemas(List<Schema> schemas) {
        List<Schema> filteredSchemas = schemas.size() > 1 ? schemas.stream()
                .filter(schema -> !"AnyType".equals(super.getSchemaType(schema))).collect(Collectors.toList())
                : schemas;

        return filteredSchemas.stream().map(schema -> getTypeDeclaration(schema)).distinct()
                .collect(Collectors.toList());
    }

    @Override
    protected void addImports(Set<String> importsToBeAddedTo, IJsonSchemaValidationProperties type) {
        Set<String> imports = type.getImports(importContainerType, importBaseType, generatorMetadata.getFeatureSet());
        Set<String> mappedImports = new HashSet<>();
        for (String imp : imports) {
            String mappedImp = imp;
            if (typeMapping.containsKey(imp)) {
                mappedImp = typeMapping.get(imp);
            } else {
                mappedImp = imp;
            }
            mappedImports.add(mappedImp);
        }
        addImports(importsToBeAddedTo, mappedImports);
    }

    @Override
    protected void addImport(Set<String> importsToBeAddedTo, String type) {
        String[] parts = splitComposedType(type);
        for (String s : parts) {
            super.addImport(importsToBeAddedTo, s);
        }
    }

    private String[] splitComposedType(String name) {
        return name.replace(" ", "").split("[|&<>]");
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);

        for (ModelMap modelMap : objs.getModels()) {
            CodegenModel cgModel = modelMap.getModel();
            if (cgModel.unescapedDescription != null && !cgModel.unescapedDescription.contains("\n * ")) {
                cgModel.description = escapeTextWhileAllowingNewLines(cgModel.unescapedDescription.trim()).replace("\n",
                        "\n * ");
            }
        }

        postProcessModelsEnum(objs); // enable enums
        return objs;
    }

    @Override
    protected void addImportsForPropertyType(CodegenModel model, CodegenProperty property) {
        if (model.getIsAnyType()) {
            return; // disable (unused) imports created for properties of type aliases
        }
        super.addImportsForPropertyType(model, property);
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);
        for (String modelName : objs.keySet()) {
            ModelsMap modelsMap = objs.get(modelName);

            // imports
            List<Map<String, String>> imports = modelsMap.getImports();
            ArrayList<Map<String, String>> n4jsImports = new ArrayList<Map<String, String>>();
            modelsMap.put("n4jsimports", n4jsImports);
            String className = modelsMap.get("classname").toString();
            for (Map<String, String> imp : imports) {
                Map<String, String> n4jsImport = toN4jsImports(className, objs, imp);
                if (n4jsImport != null) {
                    n4jsImports.add(n4jsImport);
                }
            }

            // app description -> module documentation
            adjustDescriptionWithNewLines(modelsMap);
        }
        return objs;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap operations, List<ModelMap> allModels) {
        OperationMap objs = operations.getOperations();

        boolean needImportCleanCopyBody = false;

        // The api.mustache template requires all of the auth methods for the whole api
        // Loop over all the operations and pick out each unique auth method
        Map<String, CodegenSecurity> authMethodsMap = new HashMap<>();
        for (CodegenOperation op : objs.getOperation()) {
            if (op.hasAuthMethods) {
                for (CodegenSecurity sec : op.authMethods) {
                    authMethodsMap.put(sec.name, sec);
                }
            }
            if (op.bodyParam != null && !op.bodyParam.vars.isEmpty()) {
                needImportCleanCopyBody = true;
            }
            if (op.responses != null && op.responses.size() > 0) {
                Map<String, CodegenResponse> responses2xx = new LinkedHashMap<>();
                Map<String, CodegenResponse> responses4xx = new LinkedHashMap<>();
                for (CodegenResponse response : op.responses) {
                    if (response.is2xx) {
                        responses2xx.put(response.baseType, response);
                    }
                    if (response.is4xx) {
                        responses4xx.put(response.baseType, response);
                    }
                }
                op.vendorExtensions.put("responses2xx", new ArrayList<>(responses2xx.values()));
                op.vendorExtensions.put("responses4xx", new ArrayList<>(responses4xx.values()));
            }
        }

        operations.put("needImportCleanCopyBody", needImportCleanCopyBody);

        // If there were any auth methods specified add them to the operations context
        if (!authMethodsMap.isEmpty()) {
            operations.put("authMethods", authMethodsMap.values());
            operations.put("hasAuthMethods", true);
        }

        // Add additional filename information for model imports in the apis
        Iterator<Map<String, String>> iter = operations.getImports().iterator();
        while (iter.hasNext()) {
            Map<String, String> im = iter.next();
            String className = im.get("classname");
            className = convertToModelName(className);
            String adjClassName = typeMapping.getOrDefault(className, className);
            if (needToImport(adjClassName)) {
                im.put("classname", className);
                im.put("filename", toModelImport(className));
            } else {
                iter.remove();
            }
        }

        // app description -> module documentation
        adjustDescriptionWithNewLines(additionalProperties);

        return operations;
    }

    private String convertToModelName(String modelName) {
        if (modelName == null) {
            return modelName;
        }
        Schema<?> schema = ModelUtils.getSchema(openAPI, modelName);
        if (schema == null) {
            return modelName;
        }
        if (ModelUtils.isObjectSchema(schema)) {
            return toModelFilename(modelName);
        }
        return modelName;
    }

    private void adjustDescriptionWithNewLines(Map<String, Object> map) {
        if (map.containsKey("appDescriptionWithNewLines")
                && !map.get("appDescriptionWithNewLines").toString().contains("\n * ")) {

            String appDescriptionWithNewLines = map.get("appDescriptionWithNewLines").toString();
            appDescriptionWithNewLines = appDescriptionWithNewLines.trim().replace("\n", "\n * ");
            map.put("appDescriptionWithNewLines", appDescriptionWithNewLines);
        }
    }

    private Map<String, String> toN4jsImports(String className, Map<String, ModelsMap> objs, Map<String, String> imp) {
        String modelImpName = imp.get("import");
        if (modelImpName == null) {
            return null;
        }
        String modelName = fromModelImport(modelImpName);
        if (!objs.containsKey(modelName)) {
            return null;
        }
        ModelsMap modelsMap = objs.get(modelName);
        String impClassName = modelsMap.get("classname").toString();
        if (impClassName == null || Objects.equals(impClassName, className)) {
            return null;
        }
        Map<String, String> n4jsImport = new HashMap<>();
        n4jsImport.put("elementname", impClassName);
        n4jsImport.put("modulename", modelImpName);
        return n4jsImport;
    }

    @Override
    public String toModelImport(String name) {
        if ("".equals(modelPackage())) {
            return name;
        } else {
            return modelPackage() + "/" + name;
        }
    }

    protected String fromModelImport(String modelImportName) {
        if ("".equals(modelPackage())) {
            return modelImportName;
        } else if (modelImportName == null) {
            return modelImportName;
        } else {
            if (modelImportName.startsWith(modelPackage() + "/")) {
                String nameWithoutModelPackage = modelImportName.substring(1 + modelPackage().length());
                if (modelNamePrefix != null && nameWithoutModelPackage.startsWith(modelNamePrefix)) {
                    return nameWithoutModelPackage.substring(modelNamePrefix.length());
                }
                return nameWithoutModelPackage;
            }
            return modelImportName;
        }
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            Schema<?> items = getSchemaItems((ArraySchema) p);
            return getTypeDeclaration(unaliasSchema(items)) + "[]";
        } else if (ModelUtils.isMapSchema(p)) {
            return "~Object+";
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getEnum() != null) {
                return enumValuesToEnumTypeUnion(p.getEnum(), "string");
            }
        } else if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p)) {
            // Handle integer and double enums
            if (p.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(p.getEnum()));
            }
        } else if (ModelUtils.isFileSchema(p)) {
            return "File";
        } else if (ModelUtils.isObjectSchema(p)
                || ModelUtils.isObjectSchema(ModelUtils.getReferencedSchema(openAPI, p))) {
            String result = super.getTypeDeclaration(p);
            return toModelFilename(result);
        } else if (ModelUtils.isBinarySchema(p)) {
            return "ArrayBuffer";
        }

        return super.getTypeDeclaration(p);
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    @Override
    protected String getParameterDataType(Parameter parameter, Schema p) {
        // handle enums of various data types
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema mp1 = (ArraySchema) p;
            Schema<?> inner = mp1.getItems();
            return getParameterDataType(parameter, inner) + "[]";
        } else if (ModelUtils.isMapSchema(p)) {
            return "~Object+";
        } else if (ModelUtils.isStringSchema(p)) {
            // Handle string enums
            if (p.getEnum() != null) {
                return enumValuesToEnumTypeUnion(p.getEnum(), "string");
            }
        } else if (ModelUtils.isObjectSchema(p)
                || ModelUtils.isObjectSchema(ModelUtils.getReferencedSchema(openAPI, p))) {
            String result = super.getTypeDeclaration(p);
            return toModelFilename(result);
        } else if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p)) {
            // Handle integer and double enums
            if (p.getEnum() != null) {
                return numericEnumValuesToEnumTypeUnion(new ArrayList<Number>(p.getEnum()));
            }
        }
        return this.getTypeDeclaration(p);
    }

    @Override
    protected String getSingleSchemaType(@SuppressWarnings("rawtypes") Schema schema) {
        Schema<?> unaliasSchema = unaliasSchema(schema);
        if (StringUtils.isNotBlank(unaliasSchema.get$ref())) {
            String schemaName = ModelUtils.getSimpleRef(unaliasSchema.get$ref());
            if (StringUtils.isNotEmpty(schemaName)) {
                if (schemaMapping.containsKey(schemaName)) {
                    return schemaName;
                }
            }
        }
        return super.getSingleSchemaType(unaliasSchema);
    }

    /**
     * Converts a list of strings to a literal union for representing enum values as
     * a type. Example output: 'available' | 'pending' | 'sold'
     *
     * @param values   list of allowed enum values
     * @param dataType either "string" or "number"
     * @return a literal union for representing enum values as a type
     */
    private String enumValuesToEnumTypeUnion(List<String> values, String dataType) {
        StringBuilder b = new StringBuilder();
        boolean isFirst = true;
        for (String value : values) {
            if (!isFirst) {
                b.append(" | ");
            }
            b.append(toEnumValue(value, dataType));
            isFirst = false;
        }
        return b.toString();
    }

    /**
     * Converts a list of numbers to a literal union for representing enum values as
     * a type. Example output: 3 | 9 | 55
     *
     * @param values a list of numbers
     * @return a literal union for representing enum values as a type
     */
    private String numericEnumValuesToEnumTypeUnion(List<Number> values) {
        List<String> stringValues = new ArrayList<>();
        for (Number value : values) {
            stringValues.add(value.toString());
        }
        return enumValuesToEnumTypeUnion(stringValues, "number");
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        if (property.unescapedDescription != null && property.unescapedDescription.contains("\n")) {
            property.description = escapeTextWhileAllowingNewLines(property.unescapedDescription.trim()).replace("\n",
                    "\n     * ");
        }
    }

    @Override
    public String escapeText(String input) {
        input = escapeTextWhileAllowingNewLines(input);
        if (input == null) {
            return input;
        }

        // remove \n, \r
        return input.replaceAll("[\\n\\r]", " ");
    }

    @Override
    public String escapeTextWhileAllowingNewLines(String input) {
        if (input == null) {
            return input;
        }

        // remove \t
        // outer unescape to retain the original multi-byte characters
        // finally escalate characters avoiding code injection
        return escapeUnsafeCharacters(
                StringEscapeUtils.unescapeEcmaScript(StringEscapeUtils.escapeEcmaScript(input).replace("\\/", "/"))
                        .replaceAll("[\\t]", " "));
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String toVarName(final String name) {
        String name2 = super.toVarName(name);
        for (String forbiddenChar : forbiddenChars) {
            if (name2.contains(forbiddenChar)) {
                return "[\"" + name2 + "\"]";
            }
        }
        return name2;
    }

    @Override
    public String toParamName(String name) {
        String name2 = super.toParamName(name);
        for (String forbiddenChar : forbiddenChars) {
            if (name2.contains(forbiddenChar)) {
                return "[\"" + name2 + "\"]";
            }
        }
        return name2;
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
