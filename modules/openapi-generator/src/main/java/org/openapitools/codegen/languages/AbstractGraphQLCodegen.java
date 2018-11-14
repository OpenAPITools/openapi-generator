
package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public abstract class AbstractGraphQLCodegen extends DefaultCodegen implements CodegenConfig {

    protected static final Logger LOGGER = LoggerFactory.getLogger(GraphQLSchemaGenerator.class);

    protected String specFolder = "spec";
    protected String packageName = "openapi2graphql";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected String graphQlInputsPackage = "";

    public AbstractGraphQLCodegen() {
        super();

        setReservedWordsLowerCase(
                Arrays.asList(
                        // data type
                        "null", "Int", "Float", "String", "Boolean", "ID",

                        // reserved words: TODO
                        "type", "implements", "query", "union", "interface"
                )
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "null",
                        "ID",
                        "Int",
                        "String",
                        "Float",
                        "Boolean")
        );

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Int");
        typeMapping.put("number", "Float");
        typeMapping.put("float", "Float");
        typeMapping.put("double", "Float");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("UUID", "ID");
        typeMapping.put("date", "String");
        typeMapping.put("DateTime", "String");
        typeMapping.put("password", "String");
        // TODO fix  mapping
        typeMapping.put("file", "String");
        typeMapping.put("binary", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("object", "TODO_OBJECT_MAPPING");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        }

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);
    }

    /**
     * @param packageName The new package name
     */
    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    /**
     * @param packageVersion Update package version
     */
    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    @Override
    public String escapeReservedWord(String name) {
        // Can't start with an underscore, as our fields need to start with an

        // Options?
        // - MyName
        // - AName
        // - TheName
        // - XName
        // - X_Name
        // ... or maybe a suffix?
        // - Name_ ... think this will work.
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return camelize(name) + '_';
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + packageName + File.separator + "api" + File.separator;
    }

    public String modelFileFolder() {
        return outputFolder + File.separator + packageName + File.separator + "model" + File.separator;
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = escapeReservedWord(name);

        // for reserved word or word starting with number, append _
        if (name.matches("^\\d.*"))
            name = camelize("var_" + name);

        return name;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        return camelize(toModelFilename(name));
    }

    @Override
    public String toModelFilename(String name) {
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.graphql => pet_api.graphql
        return underscore(name) + "_api";
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + "_spec";
    }

    @Override
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + "_spec";
    }

    /**
     * Overrides postProcessParameter to add a vendor extension "x-exportParamName".
     * This is useful when paramName starts with a lowercase letter, but we need that
     * param to be exportable (starts with an Uppercase letter).
     *
     * @param parameter CodegenParameter object to be processed.
     */
    @Override
    public void postProcessParameter(CodegenParameter parameter) {

    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + specFolder.replace("/", File.separator);
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + specFolder.replace("/", File.separator);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toApiName(String name) {
        return underscore(super.toApiName(name));
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            // return "[" + getTypeDeclaration(inner) + "]"; // we add this within the template because there we have to differenciate between some specific types for graphql
            return getTypeDeclaration(inner);
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            return getTypeDeclaration(inner);
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String schemaType = getSchemaType(p);
        String nullable = ModelUtils.isNullable(p) ? "" : "!";
        /*
        if (p != null && Boolean.TRUE.equals(p.getNullable())) {
            nullable = "";
        } else {
            nullable =  "!";
        }*/

        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType) + nullable;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType + nullable;
        }

        return toModelName(schemaType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else {
            type = schemaType;
        }
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + underscore("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return camelize(sanitizedOperationId, false);
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
                && !languageSpecificPrimitives.contains(type);
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("]]", "] ]");
    }

    public Map<String, String> createMapping(String key, String value) {
        Map<String, String> customImport = new HashMap<String, String>();
        customImport.put(key, value);

        return customImport;
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return escapeText(value);
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getSymbolName(name).toUpperCase(Locale.ROOT);
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (isReservedWord(enumName) || enumName.matches("\\d.*")) { // reserved word or starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = toModelName(property.name);

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toModelImport(String name) {
        if (needToImport(toModelName(name))) {
            return toModelName(name);
        }

        return name;
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");

        for (CodegenOperation op : (List<CodegenOperation>) objs.get("operation")) {
            // non GET/HEAD methods are mutation
            if (!"GET".equals(op.httpMethod.toUpperCase(Locale.ROOT)) && !"HEAD".equals(op.httpMethod.toUpperCase(Locale.ROOT))) {
                op.vendorExtensions.put("x-is-mutation", Boolean.TRUE);
            }
            for (CodegenParameter p : op.allParams) {
                // TODO check and adjust!
                // {{#vendorExtensions.x-graphql-nullable}}?{{/vendorExtensions.x-graphql-nullable}}
                if (p.required) {
                    op.vendorExtensions.put("x-graphql-nullable", Boolean.FALSE);
                } else {
                    op.vendorExtensions.put("x-graphql-nullable", Boolean.TRUE);
                }
            }
        }

        return objs;
    }

    /**
     * @return String
     */
    public String graphQlInputsPackage() {
        return graphQlInputsPackage;
    }

    /**
     * @return String
     */
    public String graphQlInputsFileFolder() {
        return outputFolder + "/" + graphQlInputsPackage().replace('.', '/');
    }

    /**
     * @param templateName
     * @param tag
     * @return String
     */
    public String graphQlInputsFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
        return graphQlInputsFileFolder() + File.separator + tographQlInputsFilename(tag) + suffix;
    }

    public String tographQlInputsFilename(String name) {
        return tographQlInputsName(name);
    }

    /**
     * @param name
     * @return String The input name
     */
    public String tographQlInputsName(String name) {
        if (name.length() == 0) {
            return "EmptyInput";
        }
        return initialCaps(name) + "Input";
    }
}