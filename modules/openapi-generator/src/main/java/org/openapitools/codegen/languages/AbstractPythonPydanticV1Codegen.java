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

import com.github.curiousoddman.rgxgen.RgxGen;
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.SecurityFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.*;

public abstract class AbstractPythonPydanticV1Codegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AbstractPythonPydanticV1Codegen.class);

    public static final String MAP_NUMBER_TO = "mapNumberTo";

    protected String packageName = "openapi_client";
    protected String packageVersion = "1.0.0";
    protected String projectName; // for setup.py, e.g. petstore-api
    protected boolean hasModelsToImport = Boolean.FALSE;
    protected String mapNumberTo = "Union[StrictFloat, StrictInt]";
    protected Map<Character, String> regexModifiers;

    private Map<String, String> schemaKeyToModelNameCache = new HashMap<>();
    // map of set (model imports)
    private HashMap<String, HashSet<String>> circularImports = new HashMap<>();
    // map of codegen models
    private HashMap<String, CodegenModel> codegenModelMap = new HashMap<>();

    public AbstractPythonPydanticV1Codegen() {
        super();

        modifyFeatureSet(features -> features.securityFeatures(EnumSet.of(
                SecurityFeature.BasicAuth,
                SecurityFeature.BearerToken,
                SecurityFeature.ApiKey,
                SecurityFeature.OAuth2_Implicit
        )));

        // from https://docs.python.org/3/reference/lexical_analysis.html#keywords
        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable name used in API methods (endpoints)
                        "all_params", "resource_path", "path_params", "query_params",
                        "header_params", "form_params", "local_var_files", "body_params", "auth_settings",
                        // @property
                        "property",
                        // python reserved words
                        "and", "del", "from", "not", "while", "as", "elif", "global", "or", "with",
                        "assert", "else", "if", "pass", "yield", "break", "except", "import",
                        "print", "class", "exec", "in", "raise", "continue", "finally", "is",
                        "return", "def", "for", "lambda", "try", "self", "nonlocal", "None", "True",
                        "False", "async", "await"));

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("list");
        languageSpecificPrimitives.add("dict");
        languageSpecificPrimitives.add("List");
        languageSpecificPrimitives.add("Dict");
        languageSpecificPrimitives.add("bool");
        languageSpecificPrimitives.add("str");
        languageSpecificPrimitives.add("datetime");
        languageSpecificPrimitives.add("date");
        languageSpecificPrimitives.add("object");
        // TODO file and binary is mapped as `file`
        languageSpecificPrimitives.add("file");
        languageSpecificPrimitives.add("bytes");

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("number", "float");
        typeMapping.put("long", "int");
        typeMapping.put("double", "float");
        typeMapping.put("array", "list");
        typeMapping.put("set", "list");
        typeMapping.put("map", "dict");
        typeMapping.put("boolean", "bool");
        typeMapping.put("string", "str");
        typeMapping.put("date", "date");
        typeMapping.put("DateTime", "datetime");
        typeMapping.put("object", "object");
        typeMapping.put("AnyType", "object");
        typeMapping.put("file", "file");
        // TODO binary should be mapped to byte array
        // mapped to String as a workaround
        typeMapping.put("binary", "str");
        typeMapping.put("ByteArray", "str");
        // map uuid to string for the time being
        typeMapping.put("UUID", "str");
        typeMapping.put("URI", "str");
        typeMapping.put("null", "none_type");

        regexModifiers = new HashMap<Character, String>();
        regexModifiers.put('i', "IGNORECASE");
        regexModifiers.put('l', "LOCALE");
        regexModifiers.put('m', "MULTILINE");
        regexModifiers.put('s', "DOTALL");
        regexModifiers.put('u', "UNICODE");
        regexModifiers.put('x', "VERBOSE");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("PYTHON_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable PYTHON_POST_PROCESS_FILE not defined so the Python code may not be properly formatted. To define it, try 'export PYTHON_POST_PROCESS_FILE=\"/usr/local/bin/yapf -i\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }


    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                if (!Boolean.valueOf(p.getDefault().toString()))
                    return "False";
                else
                    return "True";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            // TODO
        } else if (ModelUtils.isDateTimeSchema(p)) {
            // TODO
        } else if (ModelUtils.isNumberSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                String defaultValue = String.valueOf(p.getDefault());
                if (defaultValue != null) {
                    defaultValue = defaultValue.replace("\\", "\\\\")
                            .replace("'", "\'");
                    if (Pattern.compile("\r\n|\r|\n").matcher(defaultValue).find()) {
                        return "'''" + defaultValue + "'''";
                    } else {
                        return "'" + defaultValue + "'";
                    }
                }
            }
        } else if (ModelUtils.isArraySchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return null;
            }
        }

        return null;
    }


    @Override
    public String toVarName(String name) {
        // obtain the name from nameMapping directly if provided
        if (nameMapping.containsKey(name)) {
            return nameMapping.get(name);
        }

        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // remove dollar sign
        name = name.replace("$", "");

        // if it's all upper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase(Locale.ROOT);
        }

        // underscore the variable name
        // petId => pet_id
        name = underscore(name);

        // remove leading underscore
        name = name.replaceAll("^_*", "");

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toRegularExpression(String pattern) {
        return addRegularExpressionDelimiter(pattern);
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        // to avoid conflicts with 'callback' parameter for async call
        if ("callback".equals(name)) {
            return "param_callback";
        }

        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty (should not occur as an auto-generated method name will be used)
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return underscore(sanitizeName(operationId));
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // remove multiline comment
        return input.replace("'''", "'_'_'");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }
        String pythonPostProcessFile = System.getenv("PYTHON_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(pythonPostProcessFile)) {
            return; // skip if PYTHON_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with py extension
        if ("py".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = pythonPostProcessFile + " " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit value: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: {}", command);
                }
            } catch (InterruptedException | IOException e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
                // Restore interrupted state
                Thread.currentThread().interrupt();
            }
        }
    }

    @Override
    public String toExampleValue(Schema schema) {
        return toExampleValueRecursive(schema, new ArrayList<>(), 5);
    }

    private String toExampleValueRecursive(Schema schema, List<Schema> includedSchemas, int indentation) {
        boolean cycleFound = includedSchemas.stream().filter(s -> schema.equals(s)).count() > 1;
        if (cycleFound) {
            return "";
        }
        String indentationString = "";
        for (int i = 0; i < indentation; i++) indentationString += "    ";
        String example = null;
        if (schema.getExample() != null) {
            example = schema.getExample().toString();
        }

        if (ModelUtils.isNullType(schema) && null != example) {
            // The 'null' type is allowed in OAS 3.1 and above. It is not supported by OAS 3.0.x,
            // though this tooling supports it.
            return "None";
        }
        // correct "true"s into "True"s, since super.toExampleValue uses "toString()" on Java booleans
        if (ModelUtils.isBooleanSchema(schema) && null != example) {
            if ("false".equalsIgnoreCase(example)) example = "False";
            else example = "True";
        }

        // correct "&#39;"s into "'"s after toString()
        if (ModelUtils.isStringSchema(schema) && schema.getDefault() != null && !ModelUtils.isDateSchema(schema) && !ModelUtils.isDateTimeSchema(schema)) {
            example = String.valueOf(schema.getDefault());
        }

        if (StringUtils.isNotBlank(example) && !"null".equals(example)) {
            if (ModelUtils.isStringSchema(schema)) {
                example = "'" + example + "'";
            }
            return example;
        }

        if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
            // Enum case:
            example = schema.getEnum().get(0).toString();
            if (ModelUtils.isStringSchema(schema)) {
                example = "'" + escapeText(example) + "'";
            }
            if (null == example)
                LOGGER.warn("Empty enum. Cannot built an example!");

            return example;
        } else if (null != schema.get$ref()) {
            // $ref case:
            Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
            String ref = ModelUtils.getSimpleRef(schema.get$ref());
            if (allDefinitions != null) {
                Schema refSchema = allDefinitions.get(ref);
                if (null == refSchema) {
                    return "None";
                } else {
                    String refTitle = refSchema.getTitle();
                    if (StringUtils.isBlank(refTitle) || "null".equals(refTitle)) {
                        refSchema.setTitle(ref);
                    }
                    if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                        includedSchemas.add(schema);
                    }
                    return toExampleValueRecursive(refSchema, includedSchemas, indentation);
                }
            } else {
                LOGGER.warn("allDefinitions not defined in toExampleValue!\n");
            }
        }
        if (ModelUtils.isDateSchema(schema)) {
            example = "datetime.datetime.strptime('1975-12-30', '%Y-%m-%d').date()";
            return example;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            example = "datetime.datetime.strptime('2013-10-20 19:20:30.00', '%Y-%m-%d %H:%M:%S.%f')";
            return example;
        } else if (ModelUtils.isBinarySchema(schema)) {
            example = "bytes(b'blah')";
            return example;
        } else if (ModelUtils.isByteArraySchema(schema)) {
            example = "YQ==";
        } else if (ModelUtils.isStringSchema(schema)) {
            // a BigDecimal:
            if ("Number".equalsIgnoreCase(schema.getFormat())) {
                return "1";
            }
            if (StringUtils.isNotBlank(schema.getPattern())) {
                String pattern = schema.getPattern();
                RgxGen rgxGen = new RgxGen(patternCorrection(pattern));
                // this seed makes it so if we have [a-z] we pick a
                Random random = new Random(18);
                String sample = rgxGen.generate(random);
                // omit leading / and trailing /, omit trailing /i
                Pattern valueExtractor = Pattern.compile("^/\\^?(.+?)\\$?/.?$");
                Matcher m = valueExtractor.matcher(sample);
                if (m.find()) {
                    example = m.group(m.groupCount());
                } else {
                    example = sample;
                }
            }
            if (example == null) {
                example = "";
            }
            int len = 0;
            if (null != schema.getMinLength()) {
                len = schema.getMinLength().intValue();
                if (len < 1) {
                    example = "";
                } else {
                    for (int i = 0; i < len; i++) example += i;
                }
            }
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (schema.getMinimum() != null)
                example = schema.getMinimum().toString();
            else
                example = "56";
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (schema.getMinimum() != null)
                example = schema.getMinimum().toString();
            else
                example = "1.337";
        } else if (ModelUtils.isBooleanSchema(schema)) {
            example = "True";
        } else if (ModelUtils.isArraySchema(schema)) {
            if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                includedSchemas.add(schema);
            }
            ArraySchema arrayschema = (ArraySchema) schema;
            example = "[\n" + indentationString + toExampleValueRecursive(arrayschema.getItems(), includedSchemas, indentation + 1) + "\n" + indentationString + "]";
        } else if (ModelUtils.isMapSchema(schema)) {
            if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                includedSchemas.add(schema);
            }
            Object additionalObject = schema.getAdditionalProperties();
            if (additionalObject instanceof Schema) {
                Schema additional = (Schema) additionalObject;
                String theKey = "'key'";
                if (additional.getEnum() != null && !additional.getEnum().isEmpty()) {
                    theKey = additional.getEnum().get(0).toString();
                    if (ModelUtils.isStringSchema(additional)) {
                        theKey = "'" + escapeText(theKey) + "'";
                    }
                }
                example = "{\n" + indentationString + theKey + " : " + toExampleValueRecursive(additional, includedSchemas, indentation + 1) + "\n" + indentationString + "}";
            } else {
                example = "{ }";
            }
        } else if (ModelUtils.isObjectSchema(schema)) {
            if (StringUtils.isBlank(schema.getTitle())) {
                example = "None";
                return example;
            }

            // I remove any property that is a discriminator, since it is not well supported by the python generator
            String toExclude = null;
            if (schema.getDiscriminator() != null) {
                toExclude = schema.getDiscriminator().getPropertyName();
            }

            example = packageName + ".models." + underscore(schema.getTitle()) + "." + schema.getTitle() + "(";

            // if required only:
            // List<String> reqs = schema.getRequired();

            // if required and optionals
            List<String> reqs = new ArrayList<>();
            if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
                for (Object toAdd : schema.getProperties().keySet()) {
                    reqs.add((String) toAdd);
                }

                Map<String, Schema> properties = schema.getProperties();
                Set<String> propkeys = null;
                if (properties != null) propkeys = properties.keySet();
                if (toExclude != null && reqs.contains(toExclude)) {
                    reqs.remove(toExclude);
                }
                for (String toRemove : includedSchemas.stream().map(Schema::getTitle).collect(Collectors.toList())) {
                    if (reqs.contains(toRemove)) {
                        reqs.remove(toRemove);
                    }
                }
                if (StringUtils.isNotBlank(schema.getTitle()) && !"null".equals(schema.getTitle())) {
                    includedSchemas.add(schema);
                }
                if (null != schema.getRequired()) for (Object toAdd : schema.getRequired()) {
                    reqs.add((String) toAdd);
                }
                if (null != propkeys) for (String propname : propkeys) {
                    Schema schema2 = properties.get(propname);
                    if (reqs.contains(propname)) {
                        String refTitle = schema2.getTitle();
                        if (StringUtils.isBlank(refTitle) || "null".equals(refTitle)) {
                            schema2.setTitle(propname);
                        }
                        example += "\n" + indentationString + underscore(propname) + " = " +
                                toExampleValueRecursive(schema2, includedSchemas, indentation + 1) + ", ";
                    }
                }
            }
            example += ")";
        } else {
            LOGGER.debug("Type {} not handled properly in toExampleValue", schema.getType());
        }

        if (ModelUtils.isStringSchema(schema)) {
            example = "'" + escapeText(example) + "'";
        }

        return example;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            p.example = p.defaultValue;
            return;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("String".equalsIgnoreCase(type) || "str".equalsIgnoreCase(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("Integer".equals(type) || "int".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("file".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("Date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("DateTime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "'" + escapeText(example) + "'";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = this.packageName + "." + type + "()";
        } else {
            LOGGER.debug("Type {} not handled properly in setParameterExampleValue", type);
        }

        if (example == null) {
            example = "None";
        } else if (Boolean.TRUE.equals(p.isArray)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMap)) {
            example = "{'key': " + example + "}";
        }

        p.example = example;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        Schema schema = parameter.getSchema();

        if (parameter.getExample() != null) {
            codegenParameter.example = parameter.getExample().toString();
        } else if (parameter.getExamples() != null && !parameter.getExamples().isEmpty()) {
            Example example = parameter.getExamples().values().iterator().next();
            if (example.getValue() != null) {
                codegenParameter.example = example.getValue().toString();
            }
        } else if (schema != null && schema.getExample() != null) {
            codegenParameter.example = schema.getExample().toString();
        }

        setParameterExampleValue(codegenParameter);
    }

    @Override
    public String sanitizeTag(String tag) {
        return sanitizeName(tag);
    }

    public String patternCorrection(String pattern) {
        // Java does not recognize starting and ending forward slashes and mode modifiers
        // It considers them as characters with no special meaning and tries to find them in the match string
        boolean checkEnding = pattern.endsWith("/i") || pattern.endsWith("/g") || pattern.endsWith("/m");
        if (checkEnding) pattern = pattern.substring(0, pattern.length() - 2);
        if (pattern.endsWith("/")) pattern = pattern.substring(0, pattern.length() - 1);
        if (pattern.startsWith("/")) pattern = pattern.substring(1);
        return pattern;
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, this.packageName);
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        p = ModelUtils.unaliasSchema(openAPI, p);

        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            return getSchemaType(p) + "[str, " + getTypeDeclaration(inner) + "]";
        }

        String openAPIType = getSchemaType(p);
        if (typeMapping.containsKey(openAPIType)) {
            return typeMapping.get(openAPIType);
        }

        if (languageSpecificPrimitives.contains(openAPIType)) {
            return openAPIType;
        }

        return toModelName(openAPIType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type;

        if (openAPIType == null) {
            LOGGER.error("OpenAPI Type for {} is null. Default to UNKNOWN_OPENAPI_TYPE instead.", p.getName());
            openAPIType = "UNKNOWN_OPENAPI_TYPE";
        }

        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (type != null) {
                return type;
            }
        } else {
            type = openAPIType;
        }

        return toModelName(type);
    }

    @Override
    public String toModelName(String name) {
        // obtain the name from modelNameMapping directly if provided
        if (modelNameMapping.containsKey(name)) {
            return modelNameMapping.get(name);
        }

        // check if schema-mapping has a different model for this class, so we can use it
        // instead of the auto-generated one.
        if (schemaMapping.containsKey(name)) {
            return schemaMapping.get(name);
        }

        // memoization
        String origName = name;
        if (schemaKeyToModelNameCache.containsKey(origName)) {
            return schemaKeyToModelNameCache.get(origName);
        }

        String sanitizedName = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // remove dollar sign
        sanitizedName = sanitizedName.replace("$", "");
        // remove whitespace
        sanitizedName = sanitizedName.replaceAll("\\s+", "");

        String nameWithPrefixSuffix = sanitizedName;
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = modelNamePrefix + "_" + nameWithPrefixSuffix;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            // add '_' so that model name can be camelized correctly
            nameWithPrefixSuffix = nameWithPrefixSuffix + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        String camelizedName = camelize(nameWithPrefixSuffix);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            String modelName = "Model" + camelizedName; // e.g. return => ModelReturn (after camelize)
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", camelizedName, modelName);
            schemaKeyToModelNameCache.put(origName, modelName);
            return modelName;
        }

        // model name starts with number
        if (camelizedName.matches("^\\d.*")) {
            String modelName = "Model" + camelizedName; // e.g. return => ModelReturn (after camelize)
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", camelizedName, modelName);
            schemaKeyToModelNameCache.put(origName, modelName);
            return modelName;
        }

        schemaKeyToModelNameCache.put(origName, camelizedName);
        return camelizedName;
    }

    @Override
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(dropDots(toModelName(name)));
    }

    @Override
    public String toModelTestFilename(String name) {
        return "test_" + toModelFilename(name);
    }

    @Override
    public String toApiFilename(String name) {
        // e.g. PhoneNumberApi.py => phone_number_api.py
        return underscore(toApiName(name));
    }

    @Override
    public String toApiTestFilename(String name) {
        return "test_" + toApiFilename(name);
    }

    @Override
    public String toApiName(String name) {
        return super.toApiName(name);
    }

    @Override
    public String toApiVarName(String name) {
        return underscore(toApiName(name));
    }

    protected static String dropDots(String str) {
        return str.replaceAll("\\.", "_");
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.PYTHON;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        final Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        for (Map.Entry<String, ModelsMap> entry : objs.entrySet()) {
            // create hash map of codegen model
            CodegenModel cm = ModelUtils.getModelByName(entry.getKey(), objs);
            codegenModelMap.put(cm.classname, ModelUtils.getModelByName(entry.getKey(), objs));
        }

        // create circular import
        for (String m : codegenModelMap.keySet()) {
            createImportMapOfSet(m, codegenModelMap);
        }

        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            entry.setValue(postProcessModelsMap(entry.getValue()));
        }

        return processed;
    }

    private ModelsMap postProcessModelsMap(ModelsMap objs) {
        // process enum in models
        objs = postProcessModelsEnum(objs);

        TreeSet<String> typingImports = new TreeSet<>();
        TreeSet<String> pydanticImports = new TreeSet<>();
        TreeSet<String> datetimeImports = new TreeSet<>();
        TreeSet<String> modelImports = new TreeSet<>();
        TreeSet<String> postponedModelImports = new TreeSet<>();

        for (ModelMap m : objs.getModels()) {
            TreeSet<String> exampleImports = new TreeSet<>();
            TreeSet<String> postponedExampleImports = new TreeSet<>();
            List<String> readOnlyFields = new ArrayList<>();
            hasModelsToImport = false;
            int property_count = 1;
            typingImports.clear();
            pydanticImports.clear();
            datetimeImports.clear();

            CodegenModel model = m.getModel();

            // handle null type in oneOf
            if (model.getComposedSchemas() != null && model.getComposedSchemas().getOneOf() != null
                    && !model.getComposedSchemas().getOneOf().isEmpty()) {
                int index = 0;
                List<CodegenProperty> oneOfs = model.getComposedSchemas().getOneOf();
                for (CodegenProperty oneOf : oneOfs) {
                    if ("none_type".equals(oneOf.dataType)) {
                        oneOfs.remove(index);
                        break; // return earlier assuming there's only 1 null type defined
                    }
                    index++;
                }
            }

            List<CodegenProperty> codegenProperties = null;
            if (!model.oneOf.isEmpty()) { // oneOfValidationError
                codegenProperties = model.getComposedSchemas().getOneOf();
                typingImports.add("Any");
                typingImports.add("List");
                pydanticImports.add("Field");
                pydanticImports.add("StrictStr");
                pydanticImports.add("ValidationError");
                pydanticImports.add("validator");
            } else if (!model.anyOf.isEmpty()) { // anyOF
                codegenProperties = model.getComposedSchemas().getAnyOf();
                pydanticImports.add("Field");
                pydanticImports.add("StrictStr");
                pydanticImports.add("ValidationError");
                pydanticImports.add("validator");
            } else { // typical model
                codegenProperties = model.vars;

                // if super class
                if (model.getDiscriminator() != null && model.getDiscriminator().getMappedModels() != null) {
                    typingImports.add("Union");
                    Set<CodegenDiscriminator.MappedModel> discriminator = model.getDiscriminator().getMappedModels();
                    for (CodegenDiscriminator.MappedModel mappedModel : discriminator) {
                        postponedModelImports.add(mappedModel.getMappingName());
                    }
                }
            }

            if (!model.allOf.isEmpty()) { // allOf
                for (CodegenProperty cp : model.allVars) {
                    if (!cp.isPrimitiveType || cp.isModel) {
                        if (cp.isArray){ // if array
                            modelImports.add(cp.items.dataType);
                        }else{ // if model
                            modelImports.add(cp.dataType);
                        }
                    }
                }
            }

            // if model_generic.mustache is used and support additionalProperties
            if (model.oneOf.isEmpty() && model.anyOf.isEmpty()
                    && !model.isEnum
                    && !this.disallowAdditionalPropertiesIfNotPresent) {
                typingImports.add("Dict");
                typingImports.add("Any");
            }

            //loop through properties/schemas to set up typing, pydantic
            for (CodegenProperty cp : codegenProperties) {
                String typing = getPydanticType(cp, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, model.classname);
                List<String> fields = new ArrayList<>();
                String firstField = "";

                // is readOnly?
                if (cp.isReadOnly) {
                    readOnlyFields.add(cp.name);
                }

                if (!cp.required) { //optional
                    firstField = "None";
                    typing = "Optional[" + typing + "]";
                    typingImports.add("Optional");
                } else { // required
                    firstField = "...";
                    if (cp.isNullable) {
                        typing = "Optional[" + typing + "]";
                        typingImports.add("Optional");
                    }
                }

                // field
                if (cp.baseName != null && !cp.baseName.equals(cp.name)) { // base name not the same as name
                    fields.add(String.format(Locale.ROOT, "alias=\"%s\"", cp.baseName));
                }

                if (!StringUtils.isEmpty(cp.description)) { // has description
                    fields.add(String.format(Locale.ROOT, "description=\"%s\"", cp.description));
                }

                /* TODO review as example may break the build
                if (!StringUtils.isEmpty(cp.getExample())) { // has example
                    fields.add(String.format(Locale.ROOT, "example=%s", cp.getExample()));
                }*/

                String fieldCustomization;
                if ("None".equals(firstField)) {
                    if (cp.defaultValue == null) {
                        fieldCustomization = "None";
                    } else {
                        if (cp.isArray || cp.isMap) {
                            // TODO handle default value for array/map
                            fieldCustomization = "None";
                        } else {
                            fieldCustomization = cp.defaultValue;
                        }
                    }
                } else { // required field
                    fieldCustomization = firstField;
                }

                if (!fields.isEmpty()) {
                    fields.add(0, fieldCustomization);
                    pydanticImports.add("Field");
                    fieldCustomization = String.format(Locale.ROOT, "Field(%s)", StringUtils.join(fields, ", "));
                }

                if ("...".equals(fieldCustomization)) {
                    // use Field() to avoid pylint warnings
                    pydanticImports.add("Field");
                    fieldCustomization = "Field(...)";
                }

                cp.vendorExtensions.put("x-py-typing", typing + " = " + fieldCustomization);

                // setup x-py-name for each oneOf/anyOf schema
                if (!model.oneOf.isEmpty()) { // oneOf
                    cp.vendorExtensions.put("x-py-name", String.format(Locale.ROOT, "oneof_schema_%d_validator", property_count++));
                } else if (!model.anyOf.isEmpty()) { // anyOf
                    cp.vendorExtensions.put("x-py-name", String.format(Locale.ROOT, "anyof_schema_%d_validator", property_count++));
                }
            }

            // add parent model to import
            if (!StringUtils.isEmpty(model.parent)) {
                modelImports.add(model.parent);
            } else if (!model.isEnum) {
                pydanticImports.add("BaseModel");
            }

            // set enum type in extensions and update `name` in enumVars
            if (model.isEnum) {
                for (Map<String, Object> enumVars : (List<Map<String, Object>>) model.getAllowableValues().get("enumVars")) {
                    if ((Boolean) enumVars.get("isString")) {
                        model.vendorExtensions.putIfAbsent("x-py-enum-type", "str");
                        // update `name`, e.g.
                        enumVars.put("name", toEnumVariableName((String) enumVars.get("value"), "str"));
                    } else {
                        model.vendorExtensions.putIfAbsent("x-py-enum-type", "int");
                        enumVars.put("name", toEnumVariableName((String) enumVars.get("value"), "int"));
                    }
                }
            }

            // set the extensions if the key is absent
            model.getVendorExtensions().putIfAbsent("x-py-typing-imports", typingImports);
            model.getVendorExtensions().putIfAbsent("x-py-pydantic-imports", pydanticImports);
            model.getVendorExtensions().putIfAbsent("x-py-datetime-imports", datetimeImports);
            model.getVendorExtensions().putIfAbsent("x-py-readonly", readOnlyFields);

            // import models one by one
            if (!modelImports.isEmpty()) {
                Set<String> modelsToImport = new TreeSet<>();
                for (String modelImport : modelImports) {
                    if (modelImport.equals(model.classname)) {
                        // skip self import
                        continue;
                    }
                    modelsToImport.add("from " + packageName + ".models." + underscore(modelImport) + " import " + modelImport);
                }

                model.getVendorExtensions().putIfAbsent("x-py-model-imports", modelsToImport);
            }

            if (!postponedModelImports.isEmpty()) {
                Set<String> modelsToImport = new TreeSet<>();
                for (String modelImport : postponedModelImports) {
                    if (modelImport.equals(model.classname)) {
                        // skip self import
                        continue;
                    }
                    modelsToImport.add("from " + packageName + ".models." + underscore(modelImport) + " import " + modelImport);
                }

                model.getVendorExtensions().putIfAbsent("x-py-postponed-model-imports", modelsToImport);
            }

        }

        return objs;
    }


    /*
     * Gets the pydantic type given a Codegen Parameter
     *
     * @param cp codegen parameter
     * @param typingImports typing imports
     * @param pydantic pydantic imports
     * @param datetimeImports datetime imports
     * @param modelImports model imports
     * @param exampleImports example imports
     * @param postponedModelImports postponed model imports
     * @param postponedExampleImports postponed example imports
     * @param classname class name
     * @return pydantic type
     *
     */
    private String getPydanticType(CodegenParameter cp,
                                   Set<String> typingImports,
                                   Set<String> pydanticImports,
                                   Set<String> datetimeImports,
                                   Set<String> modelImports,
                                   Set<String> exampleImports,
                                   Set<String> postponedModelImports,
                                   Set<String> postponedExampleImports,
                                   String classname) {
        if (cp == null) {
            // if codegen parameter (e.g. map/dict of undefined type) is null, default to string
            LOGGER.warn("Codegen property is null (e.g. map/dict of undefined type). Default to typing.Any.");
            typingImports.add("Any");
            return "Any";
        }

        if (cp.isArray) {
            String constraints = "";
            if (cp.maxItems != null) {
                constraints += String.format(Locale.ROOT, ", max_items=%d", cp.maxItems);
            }
            if (cp.minItems != null) {
                constraints += String.format(Locale.ROOT, ", min_items=%d", cp.minItems);
            }
            if (cp.getUniqueItems()) {
                constraints += ", unique_items=True";
            }
            pydanticImports.add("conlist");
            return String.format(Locale.ROOT, "conlist(%s%s)",
                    getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, classname),
                    constraints);
        } else if (cp.isMap) {
            typingImports.add("Dict");
            return String.format(Locale.ROOT, "Dict[str, %s]",
                    getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, classname));
        } else if (cp.isString) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. constr(regex=r'/[a-z]/i', strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }
                pydanticImports.add("constr");
                return String.format(Locale.ROOT, "constr(%s)", StringUtils.join(fieldCustomization, ", "));
            } else {
                if ("password".equals(cp.getFormat())) { // TDOO avoid using format, use `is` boolean flag instead
                    pydanticImports.add("SecretStr");
                    return "SecretStr";
                } else {
                    pydanticImports.add("StrictStr");
                    return "StrictStr";
                }
            }
        } else if (cp.isNumber || cp.isFloat || cp.isDouble) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                List<String> intFieldCustomization = new ArrayList<>();

                // e.g. confloat(ge=10, le=100, strict=True)
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                        intFieldCustomization.add("lt=" + Math.ceil(Double.valueOf(cp.getMaximum()))); // e.g. < 7.59 becomes < 8
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                        intFieldCustomization.add("le=" + Math.floor(Double.valueOf(cp.getMaximum()))); // e.g. <= 7.59 becomes <= 7
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                        intFieldCustomization.add("gt=" + Math.floor(Double.valueOf(cp.getMinimum()))); // e.g. > 7.59 becomes > 7
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                        intFieldCustomization.add("ge=" + Math.ceil(Double.valueOf(cp.getMinimum()))); // e.g. >= 7.59 becomes >= 8
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    intFieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    pydanticImports.add("conint");
                    typingImports.add("Union");
                    return String.format(Locale.ROOT, "Union[%s(%s), %s(%s)]", "confloat",
                            StringUtils.join(fieldCustomization, ", "),
                            "conint",
                            StringUtils.join(intFieldCustomization, ", ")
                    );
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                } else { // float
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                }
            } else {
                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    typingImports.add("Union");
                    pydanticImports.add("StrictFloat");
                    pydanticImports.add("StrictInt");
                    return "Union[StrictFloat, StrictInt]";
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    pydanticImports.add("StrictFloat");
                    return "StrictFloat";
                } else {
                    return "float";
                }
            }
        } else if (cp.isInteger || cp.isLong || cp.isShort || cp.isUnboundedInteger) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conint(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                pydanticImports.add("conint");
                return String.format(Locale.ROOT, "%s(%s)", "conint",
                        StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("StrictInt");
                return "StrictInt";
            }
        } else if (cp.isBinary || cp.isByteArray) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conbytes(min_length=2, max_length=10)
                fieldCustomization.add("strict=True");
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(Locale.ROOT, String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }

                pydanticImports.add("conbytes");
                pydanticImports.add("constr");
                typingImports.add("Union");
                return String.format(Locale.ROOT, "Union[conbytes(%s), constr(%<s)]", StringUtils.join(fieldCustomization, ", "));
            } else {
                // same as above which has validation
                pydanticImports.add("StrictBytes");
                pydanticImports.add("StrictStr");
                typingImports.add("Union");
                return "Union[StrictBytes, StrictStr]";
            }
        } else if (cp.isBoolean) {
            pydanticImports.add("StrictBool");
            return "StrictBool";
        } else if (cp.isDecimal) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. condecimal(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("gt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("lt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }
                pydanticImports.add("condecimal");
                return String.format(Locale.ROOT, "%s(%s)", "condecimal", StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("condecimal");
                return "condecimal()";
            }
        } else if (cp.getIsAnyType()) {
            typingImports.add("Any");
            return "Any";
        } else if (cp.isDate || cp.isDateTime) {
            if (cp.isDate) {
                datetimeImports.add("date");
            }
            if (cp.isDateTime) {
                datetimeImports.add("datetime");
            }

            return cp.dataType;
        } else if (cp.isUuid) {
            return cp.dataType;
        } else if (cp.isFreeFormObject) { // type: object
            typingImports.add("Dict");
            typingImports.add("Any");
            return "Dict[str, Any]";
        } else if (!cp.isPrimitiveType) {
            // add model prefix
            hasModelsToImport = true;
            modelImports.add(cp.dataType);
            exampleImports.add(cp.dataType);
            return cp.dataType;
        } else if (cp.getContent() != null) {
            LinkedHashMap<String, CodegenMediaType> contents = cp.getContent();
            for (String key : contents.keySet()) {
                CodegenMediaType cmt = contents.get(key);
                // TODO process the first one only at the moment
                if (cmt != null)
                    return getPydanticType(cmt.getSchema(), typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, classname);
            }
            throw new RuntimeException("Error! Failed to process getPydanticType when getting the content: " + cp);
        } else {
            throw new RuntimeException("Error! Codegen Parameter not yet supported in getPydanticType: " + cp);
        }
    }


    /*
     * Gets the pydantic type given a Codegen Property
     *
     * @param cp codegen property
     * @param typingImports typing imports
     * @param pydantic pydantic imports
     * @param datetimeImports datetime imports
     * @param modelImports model imports
     * @param exampleImports example imports
     * @param postponedModelImports postponed model imports
     * @param postponedExampleImports postponed example imports
     * @param classname class name
     * @return pydantic type
     *
     */
    private String getPydanticType(CodegenProperty cp,
                                   Set<String> typingImports,
                                   Set<String> pydanticImports,
                                   Set<String> datetimeImports,
                                   Set<String> modelImports,
                                   Set<String> exampleImports,
                                   Set<String> postponedModelImports,
                                   Set<String> postponedExampleImports,
                                   String classname) {
        if (cp == null) {
            // if codegen property (e.g. map/dict of undefined type) is null, default to string
            LOGGER.warn("Codegen property is null (e.g. map/dict of undefined type). Default to typing.Any.");
            typingImports.add("Any");
            return "Any";
        }

        if (cp.isEnum) {
            pydanticImports.add("validator");
        }

        /* comment out the following since Literal requires python 3.8
           also need to put cp.isEnum check after isArray, isMap check
        if (cp.isEnum) {
            // use Literal for inline enum
            typingImports.add("Literal");
            List<String> values = new ArrayList<>();
            List<Map<String, Object>> enumVars = (List<Map<String, Object>>) cp.allowableValues.get("enumVars");
            if (enumVars != null) {
                for (Map<String, Object> enumVar : enumVars) {
                    values.add((String) enumVar.get("value"));
                }
            }
            return String.format(Locale.ROOT, "%sEnum", cp.nameInCamelCase);
        } else*/
        if (cp.isArray) {
            String constraints = "";
            if (cp.maxItems != null) {
                constraints += String.format(Locale.ROOT, ", max_items=%d", cp.maxItems);
            }
            if (cp.minItems != null) {
                constraints += String.format(Locale.ROOT, ", min_items=%d", cp.minItems);
            }
            if (cp.getUniqueItems()) {
                constraints += ", unique_items=True";
            }
            pydanticImports.add("conlist");
            typingImports.add("List"); // for return type
            return String.format(Locale.ROOT, "conlist(%s%s)",
                    getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, classname),
                    constraints);
        } else if (cp.isMap) {
            typingImports.add("Dict");
            return String.format(Locale.ROOT, "Dict[str, %s]", getPydanticType(cp.items, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, classname));
        } else if (cp.isString) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. constr(regex=r'/[a-z]/i', strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(Locale.ROOT, String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }
                pydanticImports.add("constr");
                return String.format(Locale.ROOT, "constr(%s)", StringUtils.join(fieldCustomization, ", "));
            } else {
                if ("password".equals(cp.getFormat())) { // TDOO avoid using format, use `is` boolean flag instead
                    pydanticImports.add("SecretStr");
                    return "SecretStr";
                } else {
                    pydanticImports.add("StrictStr");
                    return "StrictStr";
                }
            }
        } else if (cp.isNumber || cp.isFloat || cp.isDouble) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                List<String> intFieldCustomization = new ArrayList<>();

                // e.g. confloat(ge=10, le=100, strict=True)
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                        intFieldCustomization.add("lt=" + (int) Math.ceil(Double.valueOf(cp.getMaximum()))); // e.g. < 7.59 => < 8
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                        intFieldCustomization.add("le=" + (int) Math.floor(Double.valueOf(cp.getMaximum()))); // e.g. <= 7.59 => <= 7
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                        intFieldCustomization.add("gt=" + (int) Math.floor(Double.valueOf(cp.getMinimum()))); // e.g. > 7.59 => > 7
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                        intFieldCustomization.add("ge=" + (int) Math.ceil(Double.valueOf(cp.getMinimum()))); // e.g. >= 7.59 => >= 8
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    intFieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    pydanticImports.add("conint");
                    typingImports.add("Union");
                    return String.format(Locale.ROOT, "Union[%s(%s), %s(%s)]", "confloat",
                            StringUtils.join(fieldCustomization, ", "),
                            "conint",
                            StringUtils.join(intFieldCustomization, ", ")
                    );
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    fieldCustomization.add("strict=True");
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                } else { // float
                    pydanticImports.add("confloat");
                    return String.format(Locale.ROOT, "%s(%s)", "confloat",
                            StringUtils.join(fieldCustomization, ", "));
                }
            } else {
                if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)) {
                    typingImports.add("Union");
                    pydanticImports.add("StrictFloat");
                    pydanticImports.add("StrictInt");
                    return "Union[StrictFloat, StrictInt]";
                } else if ("StrictFloat".equals(mapNumberTo)) {
                    pydanticImports.add("StrictFloat");
                    return "StrictFloat";
                } else {
                    return "float";
                }
            }
        } else if (cp.isInteger || cp.isLong || cp.isShort || cp.isUnboundedInteger) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conint(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("lt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("gt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }

                pydanticImports.add("conint");
                return String.format(Locale.ROOT, "%s(%s)", "conint",
                        StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("StrictInt");
                return "StrictInt";
            }
        } else if (cp.isBinary || cp.isByteArray) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. conbytes(min_length=2, max_length=10)
                fieldCustomization.add("strict=True");
                if (cp.getMinLength() != null) {
                    fieldCustomization.add("min_length=" + cp.getMinLength());
                }
                if (cp.getMaxLength() != null) {
                    fieldCustomization.add("max_length=" + cp.getMaxLength());
                }
                if (cp.getPattern() != null) {
                    pydanticImports.add("validator");
                    // use validator instead as regex doesn't support flags, e.g. IGNORECASE
                    //fieldCustomization.add(Locale.ROOT, String.format(Locale.ROOT, "regex=r'%s'", cp.getPattern()));
                }

                pydanticImports.add("conbytes");
                pydanticImports.add("constr");
                typingImports.add("Union");
                return String.format(Locale.ROOT, "Union[conbytes(%s), constr(%<s)]", StringUtils.join(fieldCustomization, ", "));
            } else {
                // same as above which has validation
                pydanticImports.add("StrictBytes");
                pydanticImports.add("StrictStr");
                typingImports.add("Union");
                return "Union[StrictBytes, StrictStr]";
            }
        } else if (cp.isBoolean) {
            pydanticImports.add("StrictBool");
            return "StrictBool";
        } else if (cp.isDecimal) {
            if (cp.hasValidation) {
                List<String> fieldCustomization = new ArrayList<>();
                // e.g. condecimal(ge=10, le=100, strict=True)
                fieldCustomization.add("strict=True");
                if (cp.getMaximum() != null) {
                    if (cp.getExclusiveMaximum()) {
                        fieldCustomization.add("gt=" + cp.getMaximum());
                    } else {
                        fieldCustomization.add("ge=" + cp.getMaximum());
                    }
                }
                if (cp.getMinimum() != null) {
                    if (cp.getExclusiveMinimum()) {
                        fieldCustomization.add("lt=" + cp.getMinimum());
                    } else {
                        fieldCustomization.add("le=" + cp.getMinimum());
                    }
                }
                if (cp.getMultipleOf() != null) {
                    fieldCustomization.add("multiple_of=" + cp.getMultipleOf());
                }
                pydanticImports.add("condecimal");
                return String.format(Locale.ROOT, "%s(%s)", "condecimal", StringUtils.join(fieldCustomization, ", "));
            } else {
                pydanticImports.add("condecimal");
                return "condecimal()";
            }
        } else if (cp.getIsAnyType()) {
            typingImports.add("Any");
            return "Any";
        } else if (cp.isDate || cp.isDateTime) {
            if (cp.isDate) {
                datetimeImports.add("date");
            }
            if (cp.isDateTime) {
                datetimeImports.add("datetime");
            }
            return cp.dataType;
        } else if (cp.isUuid) {
            return cp.dataType;
        } else if (cp.isFreeFormObject) { // type: object
            typingImports.add("Dict");
            typingImports.add("Any");
            return "Dict[str, Any]";
        } else if (!cp.isPrimitiveType || cp.isModel) { // model
            // skip import if it's a circular reference
            if (classname == null) {
                // for parameter model, import directly
                hasModelsToImport = true;
                modelImports.add(cp.dataType);
                exampleImports.add(cp.dataType);
            } else {
                if (circularImports.containsKey(cp.dataType)) {
                    if (circularImports.get(cp.dataType).contains(classname)) {
                        hasModelsToImport = true;
                        postponedModelImports.add(cp.dataType);
                        postponedExampleImports.add(cp.dataType);
                        // cp.dataType import map of set contains this model (classname), don't import
                        LOGGER.debug("Skipped importing {} in {} due to circular import.", cp.dataType, classname);
                    } else {
                        // not circular import, so ok to import it
                        hasModelsToImport = true;
                        modelImports.add(cp.dataType);
                        exampleImports.add(cp.dataType);
                    }
                } else {
                    LOGGER.error("Failed to look up {} from the imports (map of set) of models.", cp.dataType);
                }
            }
            return cp.dataType;
        } else {
            throw new RuntimeException("Error! Codegen Property not yet supported in getPydanticType: " + cp);
        }
    }

    public void setMapNumberTo(String mapNumberTo) {
        if ("Union[StrictFloat, StrictInt]".equals(mapNumberTo)
                || "StrictFloat".equals(mapNumberTo)
                || "float".equals(mapNumberTo)) {
            this.mapNumberTo = mapNumberTo;
        } else {
            throw new IllegalArgumentException("mapNumberTo value must be Union[StrictFloat, StrictInt], StrictStr or float");
        }
    }

    public String toEnumVariableName(String name, String datatype) {
        if ("int".equals(datatype)) {
            return "NUMBER_" + name.replace("-", "MINUS_");
        }

        // remove quote e.g. 'abc' => abc
        name = name.substring(1, name.length() - 1);

        if (name.length() == 0) {
            return "EMPTY";
        }

        if (" ".equals(name)) {
            return "SPACE";
        }

        if ("_".equals(name)) {
            return "UNDERSCORE";
        }

        if (reservedWords.contains(name)) {
            name = name.toUpperCase(Locale.ROOT);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains(String.valueOf((char) character)))) {
            name = underscore(escape(name, specialCharReplacements, Collections.singletonList("_"), "_")).toUpperCase(Locale.ROOT);
        } else {
            name = name.toUpperCase(Locale.ROOT);
        }

        name = name.replace(" ", "_");
        name = name.replaceFirst("^_", "");
        name = name.replaceFirst("_$", "");

        if (name.matches("\\d.*")) {
            name = "ENUM_" + name.toUpperCase(Locale.ROOT);
        }

        return name;
    }

    /**
     * Update circularImports with the model name (key) and its imports gathered recursively
     *
     * @param modelName       model name
     * @param codegenModelMap a map of CodegenModel
     */
    void createImportMapOfSet(String modelName, Map<String, CodegenModel> codegenModelMap) {
        HashSet<String> imports = new HashSet<>();
        circularImports.put(modelName, imports);

        CodegenModel cm = codegenModelMap.get(modelName);

        if (cm == null) {
            LOGGER.warn("Failed to lookup model in createImportMapOfSet: " + modelName);
            return;
        }

        List<CodegenProperty> codegenProperties = null;
        if (cm.oneOf != null && !cm.oneOf.isEmpty()) { // oneOf
            codegenProperties = cm.getComposedSchemas().getOneOf();
        } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) { // anyOF
            codegenProperties = cm.getComposedSchemas().getAnyOf();
        } else { // typical model
            codegenProperties = cm.vars;
        }

        for (CodegenProperty cp : codegenProperties) {
            String modelNameFromDataType = getModelNameFromDataType(cp);
            if (modelNameFromDataType != null) { // model
                imports.add(modelNameFromDataType); // update import
                // go through properties or sub-schemas of the model recursively to identify more (model) import if any
                updateImportsFromCodegenModel(modelNameFromDataType, codegenModelMap.get(modelNameFromDataType), imports);
            }
        }
    }

    /**
     * Returns the model name (if any) from data type of codegen property.
     * Returns null if it's not a model.
     *
     * @param cp Codegen property
     * @return model name
     */
    private String getModelNameFromDataType(CodegenProperty cp) {
        if (cp.isArray) {
            return getModelNameFromDataType(cp.items);
        } else if (cp.isMap) {
            return getModelNameFromDataType(cp.items);
        } else if (!cp.isPrimitiveType || cp.isModel) {
            return cp.dataType;
        } else {
            return null;
        }
    }

    /**
     * Update set of imports from codegen model recursivly
     *
     * @param modelName model name
     * @param cm        codegen model
     * @param imports   set of imports
     */
    public void updateImportsFromCodegenModel(String modelName, CodegenModel cm, Set<String> imports) {
        if (cm == null) {
            LOGGER.warn("Failed to lookup model in createImportMapOfSet " + modelName);
            return;
        }

        List<CodegenProperty> codegenProperties = null;
        if (cm.oneOf != null && !cm.oneOf.isEmpty()) { // oneOfValidationError
            codegenProperties = cm.getComposedSchemas().getOneOf();
        } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) { // anyOF
            codegenProperties = cm.getComposedSchemas().getAnyOf();
        } else { // typical model
            codegenProperties = cm.vars;
        }

        for (CodegenProperty cp : codegenProperties) {
            String modelNameFromDataType = getModelNameFromDataType(cp);
            if (modelNameFromDataType != null) { // model
                if (modelName.equals(modelNameFromDataType)) { // self referencing
                    continue;
                } else if (imports.contains(modelNameFromDataType)) { // circular import
                    continue;
                } else {
                    imports.add(modelNameFromDataType); // update import
                    // go through properties of the model recursively to identify more (model) import if any
                    updateImportsFromCodegenModel(modelNameFromDataType, codegenModelMap.get(modelNameFromDataType), imports);
                }
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        hasModelsToImport = false;
        boolean importAnnotated = false;
        TreeSet<String> typingImports = new TreeSet<>();
        TreeSet<String> pydanticImports = new TreeSet<>();
        TreeSet<String> datetimeImports = new TreeSet<>();
        TreeSet<String> modelImports = new TreeSet<>();
        TreeSet<String> postponedModelImports = new TreeSet<>();

        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();
        for (CodegenOperation operation : operations) {
            TreeSet<String> exampleImports = new TreeSet<>(); // import for each operation to be show in sample code
            TreeSet<String> postponedExampleImports = new TreeSet<>(); // import for each operation to be show in sample code
            List<CodegenParameter> params = operation.allParams;

            for (CodegenParameter param : params) {
                String typing = getPydanticType(param, typingImports, pydanticImports, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, null);
                List<String> fields = new ArrayList<>();
                String firstField = "";

                if (!param.required) { //optional
                    firstField = "None";
                    typing = "Optional[" + typing + "]";
                    typingImports.add("Optional");
                } else { // required
                    firstField = "...";
                    if (param.isNullable) {
                        typing = "Optional[" + typing + "]";
                        typingImports.add("Optional");
                    }
                }

                if (!StringUtils.isEmpty(param.description)) { // has description
                    fields.add(String.format(Locale.ROOT, "description=\"%s\"", param.description));
                }

                /* TODO support example
                if (!StringUtils.isEmpty(cp.getExample())) { // has example
                    fields.add(String.format(Locale.ROOT, "example=%s", cp.getExample()));
                }*/

                String fieldCustomization;
                if ("None".equals(firstField)) {
                    fieldCustomization = null;
                } else { // required field
                    fieldCustomization = firstField;
                }

                if (!fields.isEmpty()) {
                    if (fieldCustomization != null) {
                        fields.add(0, fieldCustomization);
                    }
                    pydanticImports.add("Field");
                    fieldCustomization = String.format(Locale.ROOT, "Field(%s)", StringUtils.join(fields, ", "));
                } else {
                    fieldCustomization = "Field()";
                }

                if ("Field()".equals(fieldCustomization)) {
                    param.vendorExtensions.put("x-py-typing", typing);
                } else {
                    param.vendorExtensions.put("x-py-typing", String.format(Locale.ROOT, "Annotated[%s, %s]", typing, fieldCustomization));
                    importAnnotated = true;
                }
            }

            // update typing import for operation return type
            if (!StringUtils.isEmpty(operation.returnType)) {
                String typing = getPydanticType(operation.returnProperty, typingImports,
                        new TreeSet<>() /* skip pydantic import for return type */, datetimeImports, modelImports, exampleImports, postponedModelImports, postponedExampleImports, null);
            }

            // add import for code samples
            // import models one by one
            if (!exampleImports.isEmpty()) {
                List<String> imports = new ArrayList<>();
                for (String exampleImport : exampleImports) {
                    imports.add("from " + packageName + ".models." + underscore(exampleImport) + " import " + exampleImport);
                }
                operation.vendorExtensions.put("x-py-example-import", imports);
            }

            if (!postponedExampleImports.isEmpty()) {
                List<String> imports = new ArrayList<>();
                for (String exampleImport : postponedExampleImports) {
                    imports.add("from " + packageName + ".models." + underscore(exampleImport) + " import "
                            + exampleImport);
                }
                operation.vendorExtensions.put("x-py-example-import", imports);
            }
        }

        List<Map<String, String>> newImports = new ArrayList<>();

        if (importAnnotated) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, String.format(Locale.ROOT, "from typing_extensions import Annotated")));
            newImports.add(item);
        }

        // need datetime import
        if (!datetimeImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, "from datetime import %s\n", StringUtils.join(datetimeImports, ", ")));
            newImports.add(item);
        }

        // need pydantic imports
        if (!pydanticImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, "from pydantic import %s\n", StringUtils.join(pydanticImports, ", ")));
            newImports.add(item);
        }

        // need typing imports
        if (!typingImports.isEmpty()) {
            Map<String, String> item = new HashMap<>();
            item.put("import", String.format(Locale.ROOT, "from typing import %s\n", StringUtils.join(typingImports, ", ")));
            newImports.add(item);
        }

        // import models one by one
        if (!modelImports.isEmpty()) {
            for (String modelImport : modelImports) {
                Map<String, String> item = new HashMap<>();
                item.put("import", "from " + packageName + ".models." + underscore(modelImport) + " import " + modelImport);
                newImports.add(item);
            }
        }

        if (!postponedModelImports.isEmpty()) {
            for (String modelImport : postponedModelImports) {
                Map<String, String> item = new HashMap<>();
                item.put("import", "from " + packageName + ".models." + underscore(modelImport) + " import " + modelImport);
                newImports.add(item);
            }
        }

        // reset imports with newImports
        objs.setImports(newImports);
        return objs;
    }


    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        postProcessPattern(parameter.pattern, parameter.vendorExtensions);
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        postProcessPattern(property.pattern, property.vendorExtensions);
    }

    /*
     * The OpenAPI pattern spec follows the Perl convention and style of modifiers. Python
     * does not support this in as natural a way so it needs to convert it. See
     * https://docs.python.org/2/howto/regex.html#compilation-flags for details.
     *
     * @param pattern (the String pattern to convert from python to Perl convention)
     * @param vendorExtensions (list of custom x-* properties for extra functionality-see https://swagger.io/docs/specification/openapi-extensions/)
     * @return void
     * @throws IllegalArgumentException if pattern does not follow the Perl /pattern/modifiers convention
     *
     * Includes fix for issue #6675
     */
    public void postProcessPattern(String pattern, Map<String, Object> vendorExtensions) {
        if (pattern != null) {
            int i = pattern.lastIndexOf('/');

            // TOOD update the check below follow python convention
            //Must follow Perl /pattern/modifiers convention
            if (pattern.charAt(0) != '/' || i < 2) {
                throw new IllegalArgumentException("Pattern must follow the Perl "
                        + "/pattern/modifiers convention. " + pattern + " is not valid.");
            }

            String regex = pattern.substring(1, i).replace("'", "\\'");
            List<String> modifiers = new ArrayList<String>();

            for (char c : pattern.substring(i).toCharArray()) {
                if (regexModifiers.containsKey(c)) {
                    String modifier = regexModifiers.get(c);
                    modifiers.add(modifier);
                }
            }

            vendorExtensions.put("x-regex", regex.replace("\"", "\\\""));
            vendorExtensions.put("x-pattern", pattern.replace("\"", "\\\""));
            vendorExtensions.put("x-modifiers", modifiers);
        }
    }

    @Override
    public String addRegularExpressionDelimiter(String pattern) {
        if (StringUtils.isEmpty(pattern)) {
            return pattern;
        }

        if (!pattern.matches("^/.*")) {
            // Perform a negative lookbehind on each `/` to ensure that it is escaped.
            return "/" + pattern.replaceAll("(?<!\\\\)\\/", "\\\\/") + "/";
        }

        return pattern;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if ("int".equals(datatype) || "float".equals(datatype)) {
            return name;
        } else {
            return "\'" + name + "\'";
        }
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return "\'" + escapeText(value) + "\'";
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return value;
    }

    /**
     * checks if the data should be classified as "string" in enum
     * e.g. double in C# needs to be double-quoted (e.g. "2.8") by treating it as a string
     * In the future, we may rename this function to "isEnumString"
     *
     * @param dataType data type
     * @return true if it's a enum string
     */
    @Override
    public boolean isDataTypeString(String dataType) {
        return "str".equals(dataType);
    }
}
