/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class CLibcurlClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(CLibcurlClientCodegen.class);

    public static final String PROJECT_NAME = "projectName";
    protected String moduleName;
    protected String projectName;
    protected static final String defaultProjectName = "openapi_client";
    protected String specFolder = "spec";
    protected String libFolder = "lib";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected static int emptyMethodNameCounter = 0;

    public CLibcurlClientCodegen() {
        super();

        modelPackage = "models";
        apiPackage = "api";
        outputFolder = "generated-code" + File.separator + "C-libcurl";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".c");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-body.mustache", ".c");
        //modelDocTemplateFiles.put("model_doc.mustache", ".md");
        //apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "C-libcurl";

        // TODO add auto-generated test files
        //modelTestTemplateFiles.put("model_test.mustache", ".c");
        //apiTestTemplateFiles.put("api_test.mustache", ".c");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable names used in API methods (endpoints)

                        // c reserved keywords
                        // ref: https://en.cppreference.com/w/c/keyword
                        "auto",
                        "break",
                        "case",
                        "char",
                        "const",
                        "continue",
                        "default",
                        "do",
                        "double",
                        "else",
                        "enum",
                        "extern",
                        "float",
                        "for",
                        "goto",
                        "if",
                        "inline",
                        "int",
                        "long",
                        "register",
                        "remove",
                        "restrict",
                        "return",
                        "short",
                        "signed",
                        "sizeof",
                        "static",
                        "struct",
                        "switch",
                        "typedef",
                        "union",
                        "unsigned",
                        "void",
                        "volatile",
                        "while",
                        "_Alignas",
                        "_Alignof",
                        "_Atomic",
                        "_Bool",
                        "_Complex",
                        "_Generic",
                        "_Imaginary",
                        "_Noreturn",
                        "_Static_assert",
                        "_Thread_local")
        );

        instantiationTypes.clear();
        typeMapping.clear();
        importMapping.clear();
        languageSpecificPrimitives.clear();

        // primitives in C lang
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("short");
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("long");
        languageSpecificPrimitives.add("float");
        languageSpecificPrimitives.add("double");
        languageSpecificPrimitives.add("char");
        languageSpecificPrimitives.add("binary_t*");
        languageSpecificPrimitives.add("Object");
        languageSpecificPrimitives.add("list_t*");
        languageSpecificPrimitives.add("list");

        typeMapping.put("string", "char");
        typeMapping.put("char", "char");
        typeMapping.put("integer", "int");
        typeMapping.put("long", "long");
        typeMapping.put("float", "double");
        typeMapping.put("double", "float");
        typeMapping.put("number", "float");
        typeMapping.put("date", "char");
        typeMapping.put("DateTime", "char");
        typeMapping.put("boolean", "int");
        typeMapping.put("file", "binary_t*");
        typeMapping.put("binary", "binary_t*");
        typeMapping.put("ByteArray", "char");
        typeMapping.put("UUID", "char");
        typeMapping.put("URI", "char");
        typeMapping.put("array", "list");
        typeMapping.put("map", "list_t*");
        typeMapping.put("date-time", "char");

        // remove modelPackage and apiPackage added by default
        Iterator<CliOption> itr = cliOptions.iterator();
        while (itr.hasNext()) {
            CliOption opt = itr.next();
            if (CodegenConstants.MODEL_PACKAGE.equals(opt.getOpt()) ||
                    CodegenConstants.API_PACKAGE.equals(opt.getOpt())) {
                itr.remove();
            }
        }

        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).
                defaultValue(Boolean.TRUE.toString()));

    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("C_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable C_POST_PROCESS_FILE not defined so the C code may not be properly formatted by uncrustify (0.66 or later) or other code formatter. To define it, try `export C_POST_PROCESS_FILE=\"/usr/local/bin/uncrustify --no-backup\" && export UNCRUSTIFY_CONFIG=/path/to/uncrustify-rules.cfg` (Linux/Mac). Note: replace /path/to with the location of uncrustify-rules.cfg");
        }

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        // use constant model/api package (folder path)
        setModelPackage("models");
        setApiPackage("api");

        // root folder
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("libcurl.licence.mustache", "", "libcurl.licence"));
        supportingFiles.add(new SupportingFile("uncrustify-rules.cfg.mustache", "", "uncrustify-rules.cfg"));
        supportingFiles.add(new SupportingFile("README.md.mustache", "", "README.md"));
        // src folder
        supportingFiles.add(new SupportingFile("apiClient.c.mustache", "src", "apiClient.c"));
        supportingFiles.add(new SupportingFile("apiKey.c.mustache", "src", "apiKey.c"));
        supportingFiles.add(new SupportingFile("list.c.mustache", "src", "list.c"));
        // include folder
        supportingFiles.add(new SupportingFile("apiClient.h.mustache", "include", "apiClient.h"));
        supportingFiles.add(new SupportingFile("keyValuePair.h.mustache", "include", "keyValuePair.h"));
        supportingFiles.add(new SupportingFile("list.h.mustache", "include", "list.h"));
        // external folder
        supportingFiles.add(new SupportingFile("cJSON.licence.mustache", "external", "cJSON.licence"));
        supportingFiles.add(new SupportingFile("cJSON.c.mustache", "external", "cJSON.c"));
        supportingFiles.add(new SupportingFile("cJSON.h.mustache", "external", "cJSON.h"));

        // Object files in model folder
        supportingFiles.add(new SupportingFile("object-body.mustache", "model", "object.c"));
        supportingFiles.add(new SupportingFile("object-header.mustache", "model", "object.h"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "c";
    }

    @Override
    public String getHelp() {
        return "Generates a C (libcurl) client library (beta).";
    }


    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + "api";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + "model";
    }

    @Override
    public String apiTestFileFolder() {
        return outputFolder + File.separator + "unit-test";
    }

    @Override
    public String modelTestFileFolder() {
        return outputFolder + File.separator + "unit-test";
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
    public String getTypeDeclaration(Schema schema) {
        /* comment out below as we'll do it in the template instead
        if (ModelUtils.isArraySchema(schema)) {
            Schema inner = ((ArraySchema) schema).getItems();
            return getSchemaType(schema) + "<" + getTypeDeclaration(inner) + ">";
        } else if (ModelUtils.isMapSchema(schema)) {
            Schema inner = (Schema) schema.getAdditionalProperties();
            return getSchemaType(schema) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        */

        return super.getTypeDeclaration(schema);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p) || ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            }
        } else if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "'" + escapeText((String) p.getDefault()) + "'";
            }
        }

        return null;
    }

    @Override
    public String getSchemaType(Schema schema) {
        String openAPIType = super.getSchemaType(schema);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = openAPIType;
        }

        if (type == null) {
            return null;
        }

        return toModelName(type);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        // if it's all uppper case, convert to lower case
        if (name.matches("^[A-Z_]*$")) {
            name = name.toLowerCase(Locale.ROOT);
        }

        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        if (name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }
        name = name.replaceAll("-","_");
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
            String modelName = camelize("Model" + name);
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return underscore(name);
    }

    @Override
    public String toModelFilename(String name) {
        return underscore(toModelName(name));
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PhoneNumberApi.rb => phone_number_api.rb
        return camelize(name) + "API";
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String toApiTestFilename(String name) {
        return ("test_" + toApiFilename(name)).replaceAll("_", "-");
    }

    @Override
    public String toModelTestFilename(String name) {
        return ("test_" + toModelFilename(name)).replaceAll("_", "-");
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultApi";
        }
        // e.g. phone_number_api => PhoneNumberApi
        return camelize(name) + "API";
    }

    @Override
    public String toEnumValue(String value, String datatype) {
        value = value.replaceAll("-","_");
        if (isReservedWord(value)) {
            value = escapeReservedWord(value);
        }
        if ("Integer".equals(datatype) || "Float".equals(datatype)) {
            return value;
        } else {
            if (value.matches("\\d.*")) { // starts with number
                return escapeReservedWord(escapeText(value));
            } else {
                return escapeText(value);
            }
        }
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "EMPTY";
        }

        // number
        if ("Integer".equals(datatype) || "Float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "MINUS_");
            varName = varName.replaceAll("\\+", "PLUS_");
            varName = varName.replaceAll("\\.", "_DOT_");
            return varName;
        }

        // string
        String enumName = sanitizeName(camelize(name).toUpperCase(Locale.ROOT));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        String enumName = camelize(toModelName(property.name)).toUpperCase(Locale.ROOT);
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public String toOperationId(String operationId) {
        // rename to empty_method_name_1 (e.g.) if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            operationId = camelize("empty_method_name_" + emptyMethodNameCounter++);
            LOGGER.warn("Empty method name (operationId) found. Renamed to " + operationId);
            return operationId;
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize(sanitizeName("call_" + operationId), true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            String newOperationId = camelize(sanitizeName("call_" + operationId), true);
            LOGGER.warn(operationId + " (starting with a number) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        return camelize(sanitizeName(operationId), true);
    }

    @Override
    public String toApiImport(String name) {
        return apiPackage() + "/" + toApiFilename(name);
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return "#include \"" +"../model/" + importMapping.get(name) + ".h\"";
        } else
            return "#include \"" +"../model/" + name + ".h\"";
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

        if ("String".equals(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("Integer".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Float".equals(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("BOOLEAN".equals(type)) {
            if (example == null) {
                example = "true";
            }
        } else if ("File".equals(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "File.new('" + escapeText(example) + "')";
        } else if ("Date".equals(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "Date.parse('" + escapeText(example) + "')";
        } else if ("DateTime".equals(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "DateTime.parse('" + escapeText(example) + "')";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = moduleName + "::" + type + ".new";
        }

        if (example == null) {
            example = "nil";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "{'key' => " + example + "}";
        }

        p.example = example;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            setProjectName((escapeText(info.getTitle())));
        } else {
            setProjectName(defaultProjectName);
        }
        additionalProperties.put(PROJECT_NAME, projectName);
    }

    public void setProjectName(String projectName) {
        this.projectName = underscore(projectName.toLowerCase(Locale.ROOT));
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("=end", "=_end").replace("=begin", "=_begin");
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty cm = super.fromProperty(name,p);
        Schema ref = ModelUtils.getReferencedSchema(openAPI, p);
        if (ref != null) {
           if (ref.getEnum() != null) {
               cm.isEnum = true;
           }
        }
        return cm;
    }


    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String cPostProcessFile = System.getenv("C_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(cPostProcessFile)) {
            return; // skip if C_POST_PROCESS_FILE env variable is not defined
        }

        // only procees the following type (or we can simply rely on the file extension to check if it's a .c or .h file)
        Set<String> supportedFileType = new HashSet<String>(
                Arrays.asList(
                        "supporting-mustache",
                        "model-test",
                        "model",
                        "api-test",
                        "api"));
        if (!supportedFileType.contains(fileType)) {
            return;
        }

        // only process files with .c or .h extension
        if ("c".equals(FilenameUtils.getExtension(file.toString())) ||
                "h".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = cPostProcessFile + " " + file.toString();
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
                } else {
                    LOGGER.info("Successfully executed: " + command);
                }
            } catch (Exception e) {
                LOGGER.error("Error running the command ({}). Exception: {}", command, e.getMessage());
            }
        }
    }
}
