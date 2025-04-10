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

import com.google.common.base.CaseFormat;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache.Lambda;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.servers.Server;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.templating.mustache.EscapeChar;
import org.openapitools.codegen.utils.CamelizeOption;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.SimpleDateFormat;
import java.time.OffsetDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public abstract class AbstractJuliaCodegen extends DefaultCodegen {
    protected final Logger LOGGER = LoggerFactory.getLogger(AbstractJuliaCodegen.class);

    protected String srcPath = "src";
    protected String apiSrcPath = srcPath + "/apis/";
    protected String modelSrcPath = srcPath + "/models/";

    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    @Setter protected String packageName;
    @Setter protected Boolean exportModels;
    @Setter protected Boolean exportOperations;

    protected final DateTimeFormatter OFFSET_DATE_TIME_FORMAT = DateTimeFormatter.ISO_OFFSET_DATE_TIME;
    protected final SimpleDateFormat DATE_TIME_FORMAT = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSXXX", Locale.ROOT);
    protected final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat("yyyy-MM-dd", Locale.ROOT);
    protected final List<String> UNQUOTED_DATATYPES = Arrays.asList(
            "int",
            "integer",
            "long",
            "short",
            "byte",
            "float",
            "double",
            "number",
            "decimal",
            "boolean",
            "Int64",
            "Int32",
            "UInt8",
            "Float32",
            "Float64",
            "Bool"
    );


    public AbstractJuliaCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Union, SchemaSupportFeature.allOf,
                        SchemaSupportFeature.anyOf, SchemaSupportFeature.oneOf
                )
                .excludeWireFormatFeatures(
                        WireFormatFeature.XML
                )
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.BearerToken))
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .excludeGlobalFeatures(
                        GlobalFeature.Callbacks, GlobalFeature.Examples,
                        GlobalFeature.Produces, GlobalFeature.Consumes
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath, ClientModificationFeature.UserAgent
                )
        );

        reservedWords = new HashSet<String>(
                Arrays.asList(
                        "if", "else", "elseif", "while", "for", "begin", "end", "quote",
                        "try", "catch", "return", "local", "function", "macro", "ccall", "finally", "break", "continue",
                        "global", "module", "using", "import", "export", "const", "let", "do", "baremodule",
                        "Type", "Enum", "Any", "DataType", "Base"
                )
        );

        // Language Specific Primitives.  These types will not trigger imports by the client generator
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("Integer", "Int128", "Int64", "Int32", "Int16", "Int8", "UInt128", "UInt64", "UInt32", "UInt16", "UInt8", "Float64", "Float32", "Float16", "Char", "Vector", "Dict", "Vector{UInt8}", "Bool", "String", "Date", "DateTime", "ZonedDateTime", "Nothing", "Any")
        );

        typeMapping.clear();
        typeMapping.put("int", "Int64");
        typeMapping.put("integer", "Int64");
        typeMapping.put("long", "Int64");
        typeMapping.put("short", "Int32");
        typeMapping.put("byte", "UInt8");
        typeMapping.put("float", "Float32");
        typeMapping.put("double", "Float64");
        typeMapping.put("string", "String");
        typeMapping.put("char", "String");
        typeMapping.put("binary", "Vector{UInt8}");
        typeMapping.put("boolean", "Bool");
        typeMapping.put("number", "Float64");
        typeMapping.put("decimal", "Float64");
        typeMapping.put("array", "Vector");
        typeMapping.put("set", "Vector");
        typeMapping.put("map", "Dict");
        typeMapping.put("date", "Date");
        typeMapping.put("DateTime", "ZonedDateTime");
        typeMapping.put("File", "String");
        typeMapping.put("file", "String");
        typeMapping.put("UUID", "String");
        typeMapping.put("URI", "String");
        typeMapping.put("ByteArray", "Vector{UInt8}");
        typeMapping.put("object", "Any");
        typeMapping.put("Object", "Any");
        typeMapping.put("AnyType", "Any");
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.JULIA;
    }

    protected static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (reservedWords.contains(name)) {
            return "__" + name + "__";  // add underscores to reserved words, and also to obscure it to lessen chances of clashing with any other names
        } else {
            return name;
        }
    }

    /**
     * Location to write model files.
     */
    @Override
    public String modelFileFolder() {
        return (outputFolder + "/" + modelSrcPath).replace('/', File.separatorChar);
    }

    /**
     * Location to write api files.
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/" + apiSrcPath).replace('/', File.separatorChar);
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
    public String toModelFilename(String name) {
        return "model_" + toModelName(name);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        name = name.replaceAll("-", "_");
        return "api_" + camelize(name) + "Api";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "Api";
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        name = toVarName(name);
        CamelizeOption camelizeOption = CamelizeOption.UPPERCASE_FIRST_CHAR;
        name = camelize(sanitizeName(name), camelizeOption);
        name = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, name);
        return escapeReservedWord(name);
    }

    @Override
    public String toApiVarName(String name) {
        CamelizeOption camelizeOption = CamelizeOption.UPPERCASE_FIRST_CHAR;
        name = camelize(sanitizeName(name), camelizeOption);
        name = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, name);
        return escapeReservedWord(name);
    }

    @Override
    public String toVarName(String name) {
        // obtain the name from nameMapping directly if provided
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }

        return name;

    }

    /**
     * Sanitize name (parameter, property, method, etc)
     *
     * @param name string to be sanitize
     * @return sanitized string
     */
    @Override
    @SuppressWarnings("static-method")
    public String sanitizeName(String name) {
        if (name == null) {
            LOGGER.error("String to be sanitized is null. Default to ERROR_UNKNOWN");
            return "ERROR_UNKNOWN";
        }

        // if the name is just '$', map it to 'value', as that's sometimes used in the spec
        if ("$".equals(name)) {
            return "value";
        }

        name = name.replaceAll("\\[\\]", "");
        name = name.replaceAll("\\[", "_");
        name = name.replaceAll("\\]", "");
        name = name.replaceAll("\\(", "_");
        name = name.replaceAll("\\)", "");
        name = name.replaceAll("\\.", "_");
        name = name.replaceAll("-", "_");
        name = name.replaceAll(" ", "_");
        name = name.replaceAll("/", "_");
        return name.replaceAll("[^a-zA-Z0-9_{}]", "");
    }

    protected boolean needsVarEscape(String name) {
        return (!name.matches("[a-zA-Z0-9_]*") && !name.matches("var\".*\"")) || reservedWords.contains(name);
    }

    /**
     * Output the proper Julia model name.
     *
     * @param name the name of the model
     * @return Julia model name
     */
    @Override
    public String toModelName(final String name) {
        // obtain the name from modelNameMapping directly if provided
        if (modelNameMapping.containsKey(name)) {
            return modelNameMapping.get(name);
        }

        String result = sanitizeName(name);

        // remove dollar sign
        result = result.replaceAll("$", "");

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(result)) {
            LOGGER.warn(result + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + result));
            result = "model_" + result; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (result.matches("^\\d.*")) {
            LOGGER.warn(result + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + result));
            result = "model_" + result; // e.g. 200Response => Model200Response (after camelize)
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            result = modelNamePrefix + "_" + result;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            result = result + "_" + modelNameSuffix;
        }

        result = dropDots(result);
        // camelize the model name
        // phone_number => PhoneNumber
        result = camelize(result);

        return result;
    }

    @Override
    public String getTypeDeclaration(Schema schema) {
        if (ModelUtils.isArraySchema(schema)) {
            Schema inner = ModelUtils.getSchemaItems(schema);
            return getSchemaType(schema) + "{" + getTypeDeclaration(inner) + "}";
        } else if (ModelUtils.isSet(schema)) {
            Schema inner = ModelUtils.getAdditionalProperties(schema);
            return getSchemaType(schema) + "{" + getTypeDeclaration(inner) + "}";
        } else if (ModelUtils.isMapSchema(schema)) {
            Schema inner = ModelUtils.getAdditionalProperties(schema);
            return getSchemaType(schema) + "{String, " + getTypeDeclaration(inner) + "}";
        }
        return super.getTypeDeclaration(schema);
    }


    /**
     * Return the type declaration for a given schema
     *
     * @param schema the schema
     * @return the type declaration
     */
    @Override
    public String getSchemaType(Schema schema) {
        String openAPIType = super.getSchemaType(schema);
        String type = null;

        if (openAPIType == null) {
            LOGGER.error("OpenAPI Type for {} is null. Default to Object instead.", schema.getName());
            openAPIType = "Object";
        }

        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }

        return toModelName(type);
    }

    /**
     * Return the default value of the property
     *
     * @param schema OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            if (ModelUtils.isBooleanSchema(schema)) {
                return schema.getDefault().toString();
            } else if (ModelUtils.isDateSchema(schema)) {
                Object _default_obj = schema.getDefault();
                String _default;
                if (_default_obj instanceof Date) {
                    _default = DATE_FORMAT.format(_default_obj);
                } else {
                    _default = _default_obj.toString();
                }
                return "OpenAPI.str2date(\"" + _default + "\")";
            } else if (ModelUtils.isDateTimeSchema(schema)) {
                Object _default_obj = schema.getDefault();
                String _default;
                if (_default_obj instanceof DateTime) {
                    _default = DATE_TIME_FORMAT.format((DateTime) _default_obj);
                } else if (_default_obj instanceof OffsetDateTime) {
                    _default = OFFSET_DATE_TIME_FORMAT.format((OffsetDateTime) _default_obj);
                } else {
                    _default = _default_obj.toString();
                }
                return "OpenAPI.str2zoneddatetime(\"" + _default + "\")";
            } else if (ModelUtils.isIntegerSchema(schema) || ModelUtils.isLongSchema(schema) || ModelUtils.isNumberSchema(schema)) {
                return schema.getDefault().toString();
            } else if (ModelUtils.isStringSchema(schema)) {
                String _default = String.valueOf(schema.getDefault());
                return "\"" + _default + "\"";
            }
        }

        return "nothing";
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        // we do not generate any separate enum structure in Julia
        return value;
    }

    @Override
    public String toEnumVarName(String value, String datatype) {
        // we do not generate any separate enum structure in Julia
        return value;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("#=", "#_=").replace("=#", "=_#");
    }

    /**
     * Escape single and/or double quote to avoid code injection
     *
     * @param input String to be cleaned up
     * @return string with quotation mark removed or escaped
     */
    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("\"", "\\\"");
    }

    /**
     * Convert OAS Property schema to Codegen Property object.
     * <p>
     * The return value is cached. An internal cache is looked up to determine
     * if the CodegenProperty return value has already been instantiated for
     * the (String name, Schema schema) arguments.
     * Any subsequent processing of the CodegenModel return value must be idempotent
     * for a given (String name, Schema schema).
     *
     * @param name     name of the property
     * @param schema   OAS property schema
     * @param required true if the property is required in the next higher object schema, false otherwise
     * @return Codegen Property object
     */
    @Override
    public CodegenProperty fromProperty(String name, Schema schema, boolean required) {
        CodegenProperty property = super.fromProperty(name, schema, required);
        // if the name needs any escaping, we set it to var"name"
        if (needsVarEscape(property.name)) {
            property.name = "var\"" + property.name + "\"";
        }
        return property;
    }

    /**
     * Return the operation ID (method name)
     *
     * @param operationId operation ID
     * @return the sanitized method name
     */
    @Override
    @SuppressWarnings("static-method")
    public String toOperationId(String operationId) {
        CamelizeOption camelizeOption = CamelizeOption.UPPERCASE_FIRST_CHAR;
        operationId = camelize(super.toOperationId(operationId), camelizeOption);
        operationId = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, operationId);
        return sanitizeName(operationId);
    }

    private void changeParamNames(List<CodegenParameter> paramsList, HashSet<String> reservedNames) {
        // check if any param name clashes with type name and rename it
        for (CodegenParameter param : paramsList) {
            if (reservedNames.contains(param.paramName)) {
                do {
                    param.paramName = param.paramName + "_";
                } while (reservedNames.contains(param.paramName + "param"));
                param.paramName = param.paramName + "param";
            }
        }
    }

    @Override
    public String toRegularExpression(String pattern) {
        if (pattern == null) {
            return pattern;
        }

        pattern = escapeText(pattern);
        // escapeText unnecessarily escapes `\` such that `\.` in the regex ends up as `\\.` for example.
        // we need to restore it back by converting `\\` to `\`
        pattern = pattern.replaceAll("\\\\\\\\", "\\\\");
        return pattern;
    }

    /**
     * Convert OAS Operation object to Codegen Operation object
     *
     * @param httpMethod HTTP method
     * @param operation  OAS operation object
     * @param path       the path of the operation
     * @param servers    list of servers
     * @return Codegen Operation object
     */
    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);

        // collect all reserved names
        HashSet<String> reservedNames = new HashSet<String>();
        reservedNames.add(op.returnType);
        reservedNames.add(op.operationId);
        for (CodegenParameter param : op.allParams) {
            reservedNames.add(param.dataType);
        }

        changeParamNames(op.allParams, reservedNames);
        changeParamNames(op.bodyParams, reservedNames);
        changeParamNames(op.headerParams, reservedNames);
        changeParamNames(op.pathParams, reservedNames);
        changeParamNames(op.queryParams, reservedNames);
        changeParamNames(op.formParams, reservedNames);

        return op;
    }

    @Override
    protected ImmutableMap.Builder<String, Lambda> addMustacheLambdas() {
        return super.addMustacheLambdas()
                .put("escapeDollar", new EscapeChar("(?<!\\\\)\\$", "\\\\\\$"));
    }

    // override with any special post-processing
    @Override
    @SuppressWarnings("static-method")
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);
        return postProcessModelsEnum(objs);
    }

    /**
     * Return the enum value in the language specified format
     * e.g. status becomes "status"
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized value for enum
     */
    public String toEnumValue(String value, String datatype) {
        if (datatype != null && UNQUOTED_DATATYPES.contains(datatype)) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }
}
