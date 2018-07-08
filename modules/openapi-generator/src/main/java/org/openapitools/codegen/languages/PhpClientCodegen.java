/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.HashSet;
import java.util.regex.Matcher;

import org.apache.commons.lang3.StringUtils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.*;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PhpClientCodegen extends AbstractPhpCodegen {
    @SuppressWarnings("hiding")
    private static final Logger LOGGER = LoggerFactory.getLogger(PhpClientCodegen.class);

    public static final String COMPOSER_VENDOR_NAME = "composerVendorName";
    public static final String COMPOSER_PROJECT_NAME = "composerProjectName";
    protected String composerVendorName = null;
    protected String composerProjectName = null;

    public PhpClientCodegen() {
        super();

        // clear import mapping (from default generator) as php does not use it
        // at the moment
        importMapping.clear();

        setInvokerPackage("OpenAPI\\Client");
        setApiPackage(getInvokerPackage() + "\\" + apiDirName);
        setModelPackage(getInvokerPackage() + "\\" + modelDirName);
        setPackagePath("OpenAPIClient-php");
        supportsInheritance = true;
        setOutputDir("generated-code" + File.separator + "php");
        modelTestTemplateFiles.put("model_test.mustache", ".php");
        embeddedTemplateDir = templateDir = "php";

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        // provide primitives to mustache template
        List sortedLanguageSpecificPrimitives = new ArrayList(languageSpecificPrimitives);
        Collections.sort(sortedLanguageSpecificPrimitives);
        String primitives = "'" + StringUtils.join(sortedLanguageSpecificPrimitives, "', '") + "'";
        additionalProperties.put("primitives", primitives);

        cliOptions.add(new CliOption(COMPOSER_VENDOR_NAME, "The vendor name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. yaypets. IMPORTANT NOTE (2016/03): composerVendorName will be deprecated and replaced by gitUserId in the next openapi-generator release"));
        cliOptions.add(new CliOption(COMPOSER_PROJECT_NAME, "The project name used in the composer package name. The template uses {{composerVendorName}}/{{composerProjectName}} for the composer package name. e.g. petstore-client. IMPORTANT NOTE (2016/03): composerProjectName will be deprecated and replaced by gitRepoId in the next openapi-generator release"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.ALLOW_UNICODE_IDENTIFIERS_DESC)
                .defaultValue(Boolean.TRUE.toString()));
    }

    @Override
    public String escapeText(String input) {
        if (input != null) {
            // Trim the string to avoid leading and trailing spaces.
            return super.escapeText(input).trim();
        }
        return input;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "php";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(COMPOSER_PROJECT_NAME)) {
            this.setComposerProjectName((String) additionalProperties.get(COMPOSER_PROJECT_NAME));
        } else {
            additionalProperties.put(COMPOSER_PROJECT_NAME, composerProjectName);
        }

        if (additionalProperties.containsKey(COMPOSER_VENDOR_NAME)) {
            this.setComposerVendorName((String) additionalProperties.get(COMPOSER_VENDOR_NAME));
        } else {
            additionalProperties.put(COMPOSER_VENDOR_NAME, composerVendorName);
        }

        supportingFiles.add(new SupportingFile("ApiException.mustache", toPackagePath(invokerPackage, srcBasePath), "ApiException.php"));
        supportingFiles.add(new SupportingFile("Configuration.mustache", toPackagePath(invokerPackage, srcBasePath), "Configuration.php"));
        supportingFiles.add(new SupportingFile("ObjectSerializer.mustache", toPackagePath(invokerPackage, srcBasePath), "ObjectSerializer.php"));
        supportingFiles.add(new SupportingFile("ModelInterface.mustache", toPackagePath(modelPackage, srcBasePath), "ModelInterface.php"));
        supportingFiles.add(new SupportingFile("HeaderSelector.mustache", toPackagePath(invokerPackage, srcBasePath), "HeaderSelector.php"));
        supportingFiles.add(new SupportingFile("composer.mustache", getPackagePath(), "composer.json"));
        supportingFiles.add(new SupportingFile("README.mustache", getPackagePath(), "README.md"));
        supportingFiles.add(new SupportingFile("phpunit.xml.mustache", getPackagePath(), "phpunit.xml.dist"));
        supportingFiles.add(new SupportingFile(".travis.yml", getPackagePath(), ".travis.yml"));
        supportingFiles.add(new SupportingFile(".php_cs", getPackagePath(), ".php_cs"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", getPackagePath(), "git_push.sh"));
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    public void setComposerVendorName(String composerVendorName) {
        this.composerVendorName = composerVendorName;
    }

    public void setComposerProjectName(String composerProjectName) {
        this.composerProjectName = composerProjectName;
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            example = p.defaultValue;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("String".equalsIgnoreCase(type) || p.isString) {
            if (example == null) {
                example = "'" + p.paramName + "_example'";
            }
            example = escapeText(example);
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
        } else if ("\\SplFileObject".equalsIgnoreCase(type) || p.isFile) {
            if (example == null) {
                example = "/path/to/file.txt";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if ("\\Date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "new \\DateTime(\"" + escapeText(example) + "\")";
        } else if ("\\DateTime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "new \\DateTime(\"" + escapeText(example) + "\")";
        } else if ("object".equals(type)) {
            example = "new \\stdClass";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + getTypeDeclaration(type) + "()";
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (example == null) {
            example = "NULL";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "array(" + example + ")";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "array('key' => " + example + ")";
        }

        p.example = example;
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
            return getSymbolName(name).toUpperCase();
        }

        // string
        String enumName = sanitizeName(underscore(name).toUpperCase());
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
        String enumName = underscore(toModelName(property.name)).toUpperCase();

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            // for API test method name
            // e.g. public function test{{vendorExtensions.x-testOperationId}}()
            op.vendorExtensions.put("x-testOperationId", camelize(op.operationId));
        }
        return objs;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

}
