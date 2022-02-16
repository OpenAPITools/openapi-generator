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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.DocumentationFeature;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.dashize;

public class JavascriptFlowtypedClientCodegen extends AbstractTypeScriptClientCodegen {
    public static final String NPM_REPOSITORY = "npmRepository";

    protected String npmRepository = null;

    public JavascriptFlowtypedClientCodegen() {
        super();

        modifyFeatureSet(features -> features.includeDocumentationFeatures(DocumentationFeature.Readme));

        // clear import mapping (from default generator) as TS does not use it
        // at the moment
        importMapping.clear();

        setReservedWordsLowerCase(Arrays.asList(
                // local variable names used in API methods (endpoints)
                "varLocalPath", "queryParameters", "headerParams", "formParams", "useFormData", "varLocalDeferred",
                "requestOptions",
                // Typescript reserved words
                "abstract", "arguments", "boolean", "break", "byte",
                "case", "catch", "char", "class", "const",
                "continue", "debugger", "default", "delete", "do",
                "double", "else", "enum", "eval", "export",
                "extends", "false", "final", "finally", "float",
                "for", "function", "goto", "if", "implements",
                "import", "in", "instanceof", "int", "interface",
                "let", "long", "native", "new", "null",
                "package", "private", "protected", "public", "return",
                "short", "static", "super", "switch", "synchronized",
                "this", "throw", "throws", "transient", "true",
                "try", "typeof", "var", "void", "volatile",
                "while", "with", "yield",
                "Array", "Date", "eval", "function", "hasOwnProperty",
                "Infinity", "isFinite", "isNaN", "isPrototypeOf",
                "Math", "NaN", "Number", "Object",
                "prototype", "String", "toString", "undefined", "valueOf"));

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("string", "boolean", "number", "Array", "Object", "Date", "File", "Blob")
        );

        instantiationTypes.put("array", "Array");
        instantiationTypes.put("list", "Array");
        instantiationTypes.put("map", "Object");
        typeMapping.clear();
        typeMapping.put("array", "Array");
        typeMapping.put("map", "Object");
        typeMapping.put("List", "Array");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "string");
        typeMapping.put("int", "number");
        typeMapping.put("float", "number");
        typeMapping.put("number", "number");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("date", "Date");
        typeMapping.put("long", "number");
        typeMapping.put("short", "number");
        typeMapping.put("char", "string");
        typeMapping.put("double", "number");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "number");
        // file, binary not supported in JS client right now, using String as a workaround
        typeMapping.put("file", "string");
        typeMapping.put("binary", "string");
        typeMapping.put("ByteArray", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");

        defaultIncludes = new HashSet<>(languageSpecificPrimitives);
        outputFolder = "generated-code/javascript-flowtyped";
        embeddedTemplateDir = templateDir = "Javascript-Flowtyped";

        this.cliOptions.add(new CliOption(NPM_REPOSITORY, "Use this property to set an url your private npmRepo in the package.json"));

    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(new SupportingFile("index.mustache", "src", "index.js"));
        supportingFiles.add(new SupportingFile("api.mustache", "src", "api.js"));
        supportingFiles.add(new SupportingFile("configuration.mustache", "src", "configuration.js"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));

        addNpmPackageGeneration();
    }

    private void addNpmPackageGeneration() {

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        //Files for building our lib
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));
        supportingFiles.add(new SupportingFile("flowconfig.mustache", "", ".flowconfig"));
        supportingFiles.add(new SupportingFile("babelrc", "", ".babelrc"));
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            if (StringUtils.isBlank(npmName) && info.getTitle() != null) {
                // when projectName is not specified, generate it from info.title
                npmName = sanitizeName(dashize(info.getTitle()));
            }
            if (StringUtils.isBlank(npmVersion)) {
                // when projectVersion is not specified, use info.version
                npmVersion = escapeUnsafeCharacters(escapeQuotationMark(info.getVersion()));
            }
        }

        // default values
        if (StringUtils.isBlank(npmName)) {
            npmName = "openapi-js-client";
        }
        if (StringUtils.isBlank(npmVersion)) {
            npmVersion = "1.0.0";
        }

        additionalProperties.put(NPM_NAME, npmName);
        additionalProperties.put(NPM_VERSION, npmVersion);
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        List<Object> models = (List<Object>) postProcessModelsEnum(objs).get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel cm = (CodegenModel) mo.get("model");
            cm.imports = new TreeSet(cm.imports);
            // name enum with model name, e.g. StatusEnum => Pet.StatusEnum
            for (CodegenProperty var : cm.vars) {
                if (Boolean.TRUE.equals(var.isEnum)) {
                    var.datatypeWithEnum = var.datatypeWithEnum.replace(var.enumName, cm.classname + var.enumName);
                }
            }
            if (cm.parent != null) {
                for (CodegenProperty var : cm.allVars) {
                    if (Boolean.TRUE.equals(var.isEnum)) {
                        var.datatypeWithEnum = var.datatypeWithEnum
                                .replace(var.enumName, cm.classname + var.enumName);
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public String getName() {
        return "javascript-flowtyped";
    }

    @Override
    public String getHelp() {
        return "Generates a Javascript client library (beta) using Flow types and Fetch API.";
    }

    public String getNpmName() {
        return npmName;
    }

    public void setNpmName(String npmName) {
        this.npmName = npmName;
    }

    public String getNpmVersion() {
        return npmVersion;
    }

    public void setNpmVersion(String npmVersion) {
        this.npmVersion = npmVersion;
    }

    public String getNpmRepository() {
        return npmRepository;
    }

    public void setNpmRepository(String npmRepository) {
        this.npmRepository = npmRepository;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return GeneratorLanguage.JAVASCRIPT; }
}
