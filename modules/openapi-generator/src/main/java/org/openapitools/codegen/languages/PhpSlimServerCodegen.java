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

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.utils.ModelUtils;

import io.swagger.v3.oas.models.media.*;

import java.io.File;
import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Matcher;

public class PhpSlimServerCodegen extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage;
    protected String srcBasePath = "lib";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-server";
    protected String artifactVersion = "1.0.0";
    protected String packagePath = ""; // empty packagePath (top folder)


    private String variableNamingConvention = "camelCase";

    public PhpSlimServerCodegen() {
        super();

        // clear import mapping (from default generator) as slim does not use it
        // at the moment
        importMapping.clear();

        invokerPackage = camelize("OpenAPIServer");
        modelPackage = packagePath + "\\Models";
        apiPackage = packagePath;
        outputFolder = "generated-code" + File.separator + "slim";
        modelTemplateFiles.put("model.mustache", ".php");

        // no api files
        apiTemplateFiles.clear();

        embeddedTemplateDir = templateDir = "slim";

        setReservedWordsLowerCase(
                Arrays.asList(
                        "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class", "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif", "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval", "exit", "extends", "final", "for", "foreach", "function", "global", "goto", "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface", "isset", "list", "namespace", "new", "or", "print", "private", "protected", "public", "require", "require_once", "return", "static", "switch", "throw", "trait", "try", "unset", "use", "var", "while", "xor")
        );

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        // ref: http://php.net/manual/en/language.types.intro.php
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "boolean",
                        "int",
                        "integer",
                        "double",
                        "float",
                        "string",
                        "object",
                        "DateTime",
                        "mixed",
                        "number")
        );

        instantiationTypes.put("array", "array");
        instantiationTypes.put("map", "map");

        // ref: https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#data-types
        typeMapping = new HashMap<String, String>();
        typeMapping.put("integer", "int");
        typeMapping.put("long", "int");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("string", "string");
        typeMapping.put("byte", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("date", "\\DateTime");
        typeMapping.put("datetime", "\\DateTime");
        typeMapping.put("file", "\\SplFileObject");
        typeMapping.put("map", "map");
        typeMapping.put("array", "array");
        typeMapping.put("list", "array");
        typeMapping.put("object", "object");
        typeMapping.put("binary", "\\SplFileObject");

        supportingFiles.add(new SupportingFile("README.mustache", packagePath.replace('/', File.separatorChar), "README.md"));
        supportingFiles.add(new SupportingFile("composer.json", packagePath.replace('/', File.separatorChar), "composer.json"));
        supportingFiles.add(new SupportingFile("index.mustache", packagePath.replace('/', File.separatorChar), "index.php"));
        supportingFiles.add(new SupportingFile(".htaccess", packagePath.replace('/', File.separatorChar), ".htaccess"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "php-slim";
    }

    @Override
    public String getHelp() {
        return "Generates a PHP Slim Framework server library.";
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
        return (outputFolder + File.separator + toPackagePath(apiPackage, srcBasePath));
    }

    @Override
    public String modelFileFolder() {
        return (outputFolder + File.separator + toPackagePath(modelPackage, srcBasePath));
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getTypeDeclaration(inner) + "[]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            return getSchemaType(p) + "[string," + getTypeDeclaration(inner) + "]";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            String type = super.getTypeDeclaration(p);
            return (!languageSpecificPrimitives.contains(type))
                    ? "\\" + modelPackage + "\\" + type : type;
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return type;
            } else if (instantiationTypes.containsKey(type)) {
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
    public String getTypeDeclaration(String name) {
        if (!languageSpecificPrimitives.contains(name)) {
            return "\\" + modelPackage + "\\" + name;
        }
        return super.getTypeDeclaration(name);
    }

    @Override
    public String toDefaultValue(Schema p) {
        return "null";
    }

    public void setParameterNamingConvention(String variableNamingConvention) {
        this.variableNamingConvention = variableNamingConvention;
    }

    @Override
    public String toVarName(String name) {
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if ("camelCase".equals(variableNamingConvention)) {
            // return the name in camelCase style
            // phone_number => phoneNumber
            name = camelize(name, true);
        } else { // default to snake case
            // return the name in underscore style
            // PhoneNumber => phone_number
            name = underscore(name);
        }

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
        // model name cannot use reserved keyword
        if (isReservedWord(name)) {
            escapeReservedWord(name); // e.g. return => _return
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

    public String toPackagePath(String packageName, String basePath) {
        packageName = packageName.replace(invokerPackage, ""); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        if (basePath != null && basePath.length() > 0) {
            basePath = basePath.replaceAll("[\\\\/]?$", "") + File.separatorChar; // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.
        }

        String regFirstPathSeparator;
        if ("/".equals(File.separator)) { // for mac, linux
            regFirstPathSeparator = "^/";
        } else { // for windows
            regFirstPathSeparator = "^\\\\";
        }

        String regLastPathSeparator;
        if ("/".equals(File.separator)) { // for mac, linux
            regLastPathSeparator = "/$";
        } else { // for windows
            regLastPathSeparator = "\\\\$";
        }

        return (getPackagePath() + File.separatorChar + basePath
                // Replace period, backslash, forward slash with file separator in package name
                + packageName.replaceAll("[\\.\\\\/]", Matcher.quoteReplacement(File.separator))
                // Trim prefix file separators from package path
                .replaceAll(regFirstPathSeparator, ""))
                // Trim trailing file separators from the overall path
                .replaceAll(regLastPathSeparator + "$", "");
    }

    public String getPackagePath() {
        return packagePath;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "");
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            if (op.hasProduces) {
                // need to escape */* values because they breakes current mustaches
                List<Map<String, String>> c = op.produces;
                for (Map<String, String> mediaType : c) {
                    if ("*/*".equals(mediaType.get("mediaType"))) {
                        mediaType.put("mediaType", "*_/_*");
                    }
                }
            }
        }
        return objs;
    }

}
