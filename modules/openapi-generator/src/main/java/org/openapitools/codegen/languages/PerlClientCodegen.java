/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.regex.Matcher;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PerlClientCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(PerlClientCodegen.class);

    public static final String MODULE_NAME = "moduleName";
    public static final String MODULE_VERSION = "moduleVersion";
    protected String moduleName = "WWW::OpenAPIClient";
    protected String modulePathPart = moduleName.replaceAll("::", Matcher.quoteReplacement(File.separator));
    protected String moduleVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    protected static int emptyFunctionNameCounter = 0;

    public PerlClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.OAuth2_Implicit
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        // add multiple inheritance support (beta)
        supportsMultipleInheritance = true;

        // clear import mapping (from default generator) as perl does not use it
        // at the moment
        importMapping.clear();

        modelPackage = File.separatorChar + "Object";
        outputFolder = "generated-code" + File.separatorChar + "perl";
        modelTemplateFiles.put("object.mustache", ".pm");
        apiTemplateFiles.put("api.mustache", ".pm");
        modelTestTemplateFiles.put("object_test.mustache", ".t");
        apiTestTemplateFiles.put("api_test.mustache", ".t");
        modelDocTemplateFiles.put("object_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "perl";


        setReservedWordsLowerCase(
                Arrays.asList(
                        "else", "lock", "qw",
                        "__END__", "elsif", "lt", "qx",
                        "__FILE__", "eq", "m", "s",
                        "__LINE__", "exp", "ne", "sub",
                        "__PACKAGE__", "for", "no", "tr",
                        "and", "foreach", "or", "unless",
                        "cmp", "ge", "package", "until",
                        "continue", "gt", "q", "while",
                        "CORE", "if", "qq", "xor",
                        "do", "le", "qr", "y",
                        "return"
                )
        );

        languageSpecificPrimitives.clear();
        languageSpecificPrimitives.add("int");
        languageSpecificPrimitives.add("double");
        languageSpecificPrimitives.add("string");
        languageSpecificPrimitives.add("boolean");
        languageSpecificPrimitives.add("DateTime");
        languageSpecificPrimitives.add("ARRAY");
        languageSpecificPrimitives.add("HASH");
        languageSpecificPrimitives.add("object");

        typeMapping.clear();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("float", "double");
        typeMapping.put("double", "double");
        typeMapping.put("number", "double");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("date", "DateTime");
        typeMapping.put("DateTime", "DateTime");
        typeMapping.put("password", "string");
        typeMapping.put("array", "ARRAY");
        typeMapping.put("set", "ARRAY");
        typeMapping.put("map", "HASH");
        typeMapping.put("object", "object");
        typeMapping.put("binary", "string");
        typeMapping.put("file", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");

        cliOptions.clear();
        cliOptions.add(new CliOption(MODULE_NAME, "Perl module name (convention: CamelCase or Long::Module).").defaultValue("OpenAPIClient"));
        cliOptions.add(new CliOption(MODULE_VERSION, "Perl module version.").defaultValue("1.0.0"));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG,
                CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENSURE_UNIQUE_PARAMS, CodegenConstants
                .ENSURE_UNIQUE_PARAMS_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC)
                .defaultValue(Boolean.TRUE.toString()));

        // option to change the order of form/body parameter
        cliOptions.add(CliOption.newBoolean(
                CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS,
                CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS_DESC)
                .defaultValue(Boolean.FALSE.toString()));

    }


    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("PERL_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable PERL_POST_PROCESS_FILE not defined so the Perl code may not be properly formatted. To define it, try 'export PERL_POST_PROCESS_FILE=/usr/local/bin/perltidy -b -bext=\"/\"' (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(MODULE_VERSION)) {
            setModuleVersion((String) additionalProperties.get(MODULE_VERSION));
        } else {
            additionalProperties.put(MODULE_VERSION, moduleVersion);
        }

        if (additionalProperties.containsKey(MODULE_NAME)) {
            setModuleName((String) additionalProperties.get(MODULE_NAME));
            setModulePathPart(moduleName.replaceAll("::", Matcher.quoteReplacement(File.separator)));
        } else {
            additionalProperties.put(MODULE_NAME, moduleName);
        }

        // make api and model doc path available in mustache template
        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        supportingFiles.add(new SupportingFile("ApiClient.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "ApiClient.pm"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "Configuration.pm"));
        supportingFiles.add(new SupportingFile("ApiFactory.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "ApiFactory.pm"));
        supportingFiles.add(new SupportingFile("Role.mustache", ("lib/" + modulePathPart).replace('/', File.separatorChar), "Role.pm"));
        supportingFiles.add(new SupportingFile("AutoDoc.mustache", ("lib/" + modulePathPart + "/Role").replace('/', File.separatorChar), "AutoDoc.pm"));
        supportingFiles.add(new SupportingFile("autodoc.script.mustache", "bin", "autodoc"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "perl";
    }

    @Override
    public String getHelp() {
        return "Generates a Perl client library.";
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
        return (outputFolder + "/lib/" + modulePathPart + apiPackage()).replace('/', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + "/lib/" + modulePathPart + modelPackage()).replace('/', File.separatorChar);
    }

    @Override
    public String apiTestFileFolder() {
        return (outputFolder + "/t").replace('/', File.separatorChar);
    }

    @Override
    public String modelTestFileFolder() {
        return (outputFolder + "/t").replace('/', File.separatorChar);
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
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);
            return getSchemaType(p) + "[string," + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            }
        } else {
            type = schemaType;
        }
        if (type == null) {
            return null;
        }
        return toModelName(type);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
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
                return "'" + p.getDefault() + "'";
            }
        }

        return null;
    }


    @Override
    public String toVarName(String name) {
        // return the name in underscore style
        // PhoneNumber => phone_number
        name = underscore(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // parameter name starting with number won't compile
        // need to escape it by appending _ at the beginning
        if (name.matches("^\\d.*")) {
            name = "_" + name;
        }
        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // model name cannot use reserved keyword
        if (isReservedWord(name)) {
            LOGGER.warn("{} (reserved word) cannot be used as model name. Renamed to {}", name, camelize("model_" + name));
            name = "model_" + name;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn("{} (model name starts with number) cannot be used as model name. Renamed to {}", name,
                    camelize("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        // add prefix/suffix to model name
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
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
    public String toModelTestFilename(String name) {
        return toModelFilename(name) + "Test";
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelFilename(name);
    }

    @Override
    public String toApiTestFilename(String name) {
        return toApiFilename(name) + "Test";
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiFilename(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. phone_number_api.rb => PhoneNumberApi.rb
        return camelize(name) + "Api";
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
    public String toOperationId(String operationId) {
        //rename to empty_function_name_1 (e.g.) if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            operationId = underscore("empty_function_name_" + emptyFunctionNameCounter++);
            LOGGER.warn("Empty method name (operationId) found. Renamed to {}", operationId);
            return operationId;
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn("{} (reserved word) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)));
            return underscore(sanitizeName("call_" + operationId));
        }

        // operationId starts with a number
        if (operationId.matches("^\\d.*")) {
            LOGGER.warn("{} (starting with a number) cannot be used as method name. Renamed to {}", operationId, underscore(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        //return underscore(operationId).replaceAll("[^A-Za-z0-9_]", "");
        return underscore(sanitizeName(operationId));
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public void setModulePathPart(String modulePathPart) {
        this.modulePathPart = modulePathPart;
    }

    public void setModuleVersion(String moduleVersion) {
        this.moduleVersion = moduleVersion;
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

        if (Boolean.TRUE.equals(p.isInteger)) {
            if (example == null) {
                example = "56";
            }
        } else if (Boolean.TRUE.equals(p.isLong)) {
            if (example == null) {
                example = "789";
            }
        } else if (Boolean.TRUE.equals(p.isDouble)
                || Boolean.TRUE.equals(p.isFloat)
                || Boolean.TRUE.equals(p.isNumber)) {
            if (example == null) {
                example = "3.4";
            }
        } else if (Boolean.TRUE.equals(p.isBoolean)) {
            if (Boolean.parseBoolean(p.example)) {
                p.example = "1";
            } else {
                p.example = "0";
            }
        } else if (Boolean.TRUE.equals(p.isFile) || Boolean.TRUE.equals(p.isBinary)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (Boolean.TRUE.equals(p.isByteArray)) {
            if (example == null) {
                example = "YmFzZSA2NCBkYXRh";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "DateTime->from_epoch(epoch => str2time('" + escapeText(p.example) + "'))";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "DateTime->from_epoch(epoch => str2time('" + escapeText(p.example) + "'))";
        } else if (Boolean.TRUE.equals(p.isString)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "\"" + escapeText(example) + "\"";

        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + moduleName + "." + type + "()";
        }

        // container
        if (Boolean.TRUE.equals(p.isArray)) {
            example = setPropertyExampleValue(p.items);
            example = "(" + example + ")";
        } else if (Boolean.TRUE.equals(p.isMap)) {
            example = setPropertyExampleValue(p.items);
            example = "('key' =>  " + example + "}";
        } else if (example == null) {
            example = "null";
        }

        p.example = example;
    }

    protected String setPropertyExampleValue(CodegenProperty p) {
        String example;

        if (p == null) {
            return "null";
        }

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if (Boolean.TRUE.equals(p.isInteger)) {
            if (example == null) {
                example = "56";
            }
        } else if (Boolean.TRUE.equals(p.isLong)) {
            if (example == null) {
                example = "789";
            }
        } else if (Boolean.TRUE.equals(p.isDouble)
                || Boolean.TRUE.equals(p.isFloat)
                || Boolean.TRUE.equals(p.isNumber)) {
            if (example == null) {
                example = "3.4";
            }
        } else if (Boolean.TRUE.equals(p.isBoolean)) {
            if (example == null) {
                example = "true";
            }
        } else if (Boolean.TRUE.equals(p.isFile) || Boolean.TRUE.equals(p.isBinary)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if (Boolean.TRUE.equals(p.isDate)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "DateTime->from_epoch(epoch => str2time('" + escapeText(p.example) + "'))";
        } else if (Boolean.TRUE.equals(p.isDateTime)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "DateTime->from_epoch(epoch => str2time('" + escapeText(p.example) + "'))";
        } else if (Boolean.TRUE.equals(p.isString)) {
            if (example == null) {
                example = p.name + "_example";
            }
            example = "\"" + escapeText(example) + "\"";

        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + moduleName + "." + type + "()";
        }

        return example;
    }

    @Override
    public String escapeQuotationMark(String input) {
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // remove =end, =cut to avoid code injection
        return input.replace("=begin", "=_begin").replace("=end", "=_end").replace("=cut", "=_cut").replace("=pod", "=_pod");
    }

    @Override
    public void postProcessFile(File file, String fileType) {
        if (file == null) {
            return;
        }

        String perlTidyPath = System.getenv("PERL_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(perlTidyPath)) {
            return; // skip if PERL_POST_PROCESS_FILE env variable is not defined
        }

        // only process files with .t, .pm extension
        if ("t".equals(FilenameUtils.getExtension(file.toString())) ||
                "pm".equals(FilenameUtils.getExtension(file.toString())) ||
                "pl".equals(FilenameUtils.getExtension(file.toString()))) {
            String command = perlTidyPath + " -b -bext='/' " + file;
            try {
                Process p = Runtime.getRuntime().exec(command);
                int exitValue = p.waitFor();
                if (exitValue != 0) {
                    LOGGER.error("Error running the command ({}). Exit code: {}", command, exitValue);
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
    public void postProcess() {
        System.out.println("################################################################################");
        System.out.println("# Thanks for using OpenAPI Generator.                                          #");
        System.out.println("# Please consider donation to help us maintain this project \uD83D\uDE4F                 #");
        System.out.println("# https://opencollective.com/openapi_generator/donate                          #");
        System.out.println("#                                                                              #");
        System.out.println("# This generator is created by wing328 (https://github.com/wing328)            #");
        System.out.println("# Please support his work directly by purchasing a copy of the eBook \ud83d\udcd8        #");
        System.out.println("# - OpenAPI Generator for Perl Developers            https://bit.ly/2OId6p3    #");
        System.out.println("################################################################################");
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.PERL; }
}
