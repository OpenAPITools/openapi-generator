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

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.SemVer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.*;

public class TypeScriptAngularClientCodegen extends AbstractTypeScriptClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(TypeScriptAngularClientCodegen.class);

    private static final SimpleDateFormat SNAPSHOT_SUFFIX_FORMAT = new SimpleDateFormat("yyyyMMddHHmm");
    private static final String X_DISCRIMINATOR_TYPE = "x-discriminator-value";

    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String SNAPSHOT = "snapshot";
    public static final String WITH_INTERFACES = "withInterfaces";
    public static final String TAGGED_UNIONS = "taggedUnions";
    public static final String NG_VERSION = "ngVersion";
    public static final String PROVIDED_IN_ROOT ="providedInRoot";


    protected String npmName = null;
    protected String npmVersion = "1.0.0";
    protected String npmRepository = null;

    private boolean taggedUnions = false;

    public TypeScriptAngularClientCodegen() {
        super();
        this.outputFolder = "generated-code/typescript-angular";

        embeddedTemplateDir = templateDir = "typescript-angular";
        modelTemplateFiles.put("model.mustache", ".ts");
        apiTemplateFiles.put("api.service.mustache", ".ts");
        languageSpecificPrimitives.add("Blob");
        typeMapping.put("file", "Blob");
        apiPackage = "api";
        modelPackage = "model";

        this.cliOptions.add(new CliOption(NPM_NAME, "The name under which you want to publish generated npm package." +
                " Required to generate a full angular package"));
        this.cliOptions.add(new CliOption(NPM_VERSION, "The version of your npm package. Default is '1.0.0'"));
        this.cliOptions.add(new CliOption(NPM_REPOSITORY,
                "Use this property to set an url your private npmRepo in the package.json"));
        this.cliOptions.add(new CliOption(SNAPSHOT,
                "When setting this property to true the version will be suffixed with -SNAPSHOT.yyyyMMddHHmm",
                SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(WITH_INTERFACES,
                "Setting this property to true will generate interfaces next to the default class implementations.",
                SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(TAGGED_UNIONS,
                "Use discriminators to create tagged unions instead of extending interfaces.",
                SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(PROVIDED_IN_ROOT,
                "Use this property to provide Injectables in root (it is only valid in angular version greater or equal to 6.0.0).",
                SchemaTypeUtil.BOOLEAN_TYPE).defaultValue(Boolean.FALSE.toString()));
        this.cliOptions.add(new CliOption(NG_VERSION, "The version of Angular. Default is '4.3'"));
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration((Schema) schema.getAdditionalProperties());
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    public String getName() {
        return "typescript-angular";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript Angular (2.x - 5.x) client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.add(
                new SupportingFile("models.mustache", modelPackage().replace('.', File.separatorChar), "models.ts"));
        supportingFiles
                .add(new SupportingFile("apis.mustache", apiPackage().replace('.', File.separatorChar), "api.ts"));
        supportingFiles.add(new SupportingFile("index.mustache", getIndexDirectory(), "index.ts"));
        supportingFiles.add(new SupportingFile("api.module.mustache", getIndexDirectory(), "api.module.ts"));
        supportingFiles.add(new SupportingFile("configuration.mustache", getIndexDirectory(), "configuration.ts"));
        supportingFiles.add(new SupportingFile("variables.mustache", getIndexDirectory(), "variables.ts"));
        supportingFiles.add(new SupportingFile("encoder.mustache", getIndexDirectory(), "encoder.ts"));
        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("README.mustache", getIndexDirectory(), "README.md"));

        // determine NG version
        SemVer ngVersion;
        if (additionalProperties.containsKey(NG_VERSION)) {
            ngVersion = new SemVer(additionalProperties.get(NG_VERSION).toString());
        } else {
            ngVersion = new SemVer("4.3.0");
            LOGGER.info("generating code for Angular {} ...", ngVersion);
            LOGGER.info("  (you can select the angular version by setting the additionalProperty ngVersion)");
        }

        if (additionalProperties.containsKey(NPM_NAME)) {
            addNpmPackageGeneration(ngVersion);
        }

        if (additionalProperties.containsKey(WITH_INTERFACES)) {
            boolean withInterfaces = Boolean.parseBoolean(additionalProperties.get(WITH_INTERFACES).toString());
            if (withInterfaces) {
                apiTemplateFiles.put("apiInterface.mustache", "Interface.ts");
            }
        }

        if (additionalProperties.containsKey(TAGGED_UNIONS)) {
            taggedUnions = Boolean.parseBoolean(additionalProperties.get(TAGGED_UNIONS).toString());
        }

        if (ngVersion.atLeast("6.0.0")) {
            if (!additionalProperties.containsKey(PROVIDED_IN_ROOT)){
                additionalProperties.put(PROVIDED_IN_ROOT,true);
            }else {
                additionalProperties.put(PROVIDED_IN_ROOT,Boolean.valueOf(
                    (String) additionalProperties.get(PROVIDED_IN_ROOT)));
            }
        }else {
            additionalProperties.put(PROVIDED_IN_ROOT,false);
        }

        additionalProperties.put(NG_VERSION, ngVersion);
        additionalProperties.put("injectionToken", ngVersion.atLeast("4.0.0") ? "InjectionToken" : "OpaqueToken");
        additionalProperties.put("injectionTokenTyped", ngVersion.atLeast("4.0.0"));
        additionalProperties.put("useHttpClient", ngVersion.atLeast("4.3.0"));
        additionalProperties.put("useRxJS6", ngVersion.atLeast("6.0.0"));
        if (!ngVersion.atLeast("4.3.0")) {
            supportingFiles.add(new SupportingFile("rxjs-operators.mustache", getIndexDirectory(), "rxjs-operators.ts"));
        }
    }

    private void addNpmPackageGeneration(SemVer ngVersion) {

        if (additionalProperties.containsKey(NPM_NAME)) {
            this.setNpmName(additionalProperties.get(NPM_NAME).toString());
        }

        if (additionalProperties.containsKey(NPM_VERSION)) {
            this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
        }

        if (additionalProperties.containsKey(SNAPSHOT)
                && Boolean.valueOf(additionalProperties.get(SNAPSHOT).toString())) {
            this.setNpmVersion(npmVersion + "-SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.format(new Date()));
        }
        additionalProperties.put(NPM_VERSION, npmVersion);

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        // for Angular 2 AOT support we will use good-old ngc,
        // Angular Package format wasn't invented at this time and building was much more easier
        if (!ngVersion.atLeast("4.0.0")) {
            LOGGER.warn("Please update your legacy Angular " + ngVersion + " project to benefit from 'Angular Package Format' support.");
            additionalProperties.put("useNgPackagr", false);
        } else {
            additionalProperties.put("useNgPackagr", true);
            supportingFiles.add(new SupportingFile("ng-package.mustache", getIndexDirectory(), "ng-package.json"));
        }

        // Libraries generated with v1.x of ng-packagr will ship with AoT metadata in v3, which is intended for Angular v4.
        // Libraries generated with v2.x of ng-packagr will ship with AoT metadata in v4, which is intended for Angular v5 (and Angular v6).
        additionalProperties.put("useOldNgPackagr", !ngVersion.atLeast("5.0.0"));

        //Files for building our lib
        supportingFiles.add(new SupportingFile("package.mustache", getIndexDirectory(), "package.json"));
        supportingFiles.add(new SupportingFile("typings.mustache", getIndexDirectory(), "typings.json"));
        supportingFiles.add(new SupportingFile("tsconfig.mustache", getIndexDirectory(), "tsconfig.json"));
    }

    private String getIndexDirectory() {
        String indexPackage = modelPackage.substring(0, Math.max(0, modelPackage.lastIndexOf('.')));
        return indexPackage.replace('.', File.separatorChar);
    }

    @Override
    public boolean isDataTypeFile(final String dataType) {
        return dataType != null && dataType.equals("Blob");
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isFileSchema(p)) {
            return "Blob";
        } else {
            return super.getTypeDeclaration(p);
        }
    }


    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        if (isLanguagePrimitive(openAPIType) || isLanguageGenericType(openAPIType)) {
            return openAPIType;
        }
        applyLocalTypeMapping(openAPIType);
        return openAPIType;
    }

    private String applyLocalTypeMapping(String type) {
        if (typeMapping.containsKey(type)) {
            type = typeMapping.get(type);
        }
        return type;
    }

    private boolean isLanguagePrimitive(String type) {
        return languageSpecificPrimitives.contains(type);
    }

    private boolean isLanguageGenericType(String type) {
        for (String genericType : languageGenericTypes) {
            if (type.startsWith(genericType + "<")) {
                return true;
            }
        }
        return false;
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);
        parameter.dataType = applyLocalTypeMapping(parameter.dataType);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> operations) {
        Map<String, Object> objs = (Map<String, Object>) operations.get("operations");

        // Add filename information for api imports
        objs.put("apiFilename", getApiFilenameFromClassname(objs.get("classname").toString()));

        List<CodegenOperation> ops = (List<CodegenOperation>) objs.get("operation");
        for (CodegenOperation op : ops) {
            if ((boolean) additionalProperties.get("useHttpClient")) {
                op.httpMethod = op.httpMethod.toLowerCase(Locale.ENGLISH);
            } else {
                // Convert httpMethod to Angular's RequestMethod enum
                // https://angular.io/docs/ts/latest/api/http/index/RequestMethod-enum.html
                switch (op.httpMethod) {
                    case "GET":
                        op.httpMethod = "RequestMethod.Get";
                        break;
                    case "POST":
                        op.httpMethod = "RequestMethod.Post";
                        break;
                    case "PUT":
                        op.httpMethod = "RequestMethod.Put";
                        break;
                    case "DELETE":
                        op.httpMethod = "RequestMethod.Delete";
                        break;
                    case "OPTIONS":
                        op.httpMethod = "RequestMethod.Options";
                        break;
                    case "HEAD":
                        op.httpMethod = "RequestMethod.Head";
                        break;
                    case "PATCH":
                        op.httpMethod = "RequestMethod.Patch";
                        break;
                    default:
                        throw new RuntimeException("Unknown HTTP Method " + op.httpMethod + " not allowed");
                }
            }

            // Prep a string buffer where we're going to set up our new version of the string.
            StringBuilder pathBuffer = new StringBuilder();
            StringBuilder parameterName = new StringBuilder();
            int insideCurly = 0;

            // Iterate through existing string, one character at a time.
            for (int i = 0; i < op.path.length(); i++) {
                switch (op.path.charAt(i)) {
                    case '{':
                        // We entered curly braces, so track that.
                        insideCurly++;

                        // Add the more complicated component instead of just the brace.
                        pathBuffer.append("${encodeURIComponent(String(");
                        break;
                    case '}':
                        // We exited curly braces, so track that.
                        insideCurly--;

                        // Add the more complicated component instead of just the brace.
                        CodegenParameter parameter = findPathParameterByName(op, parameterName.toString());
                        pathBuffer.append(toVarName(parameterName.toString()));
                        if (parameter != null && parameter.isDateTime) {
                            pathBuffer.append(".toISOString()");
                        }
                        pathBuffer.append("))}");
                        parameterName.setLength(0);
                        break;
                    default:
                        char nextChar = op.path.charAt(i);
                        if (insideCurly > 0) {
                            parameterName.append(nextChar);
                        } else {
                            pathBuffer.append(nextChar);
                        }
                        break;
                }
            }

            // Overwrite path to TypeScript template string, after applying everything we just did.
            op.path = pathBuffer.toString();
        }

        // Add additional filename information for model imports in the services
        List<Map<String, Object>> imports = (List<Map<String, Object>>) operations.get("imports");
        for (Map<String, Object> im : imports) {
            im.put("filename", im.get("import"));
            im.put("classname", getModelnameFromModelFilename(im.get("filename").toString()));
        }

        return operations;
    }

    /**
     * Finds and returns a path parameter of an operation by its name
     * @param operation
     * @param parameterName
     * @return
     */
    private CodegenParameter findPathParameterByName(CodegenOperation operation, String parameterName) {
        for(CodegenParameter param : operation.pathParams) {
            if (param.baseName.equals(parameterName)) {
                return param;
            }
        }
        return null;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessModels(objs);

        return postProcessModelsEnum(result);
    }

    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        Map<String, Object> result = super.postProcessAllModels(objs);

        for (Map.Entry<String, Object> entry : result.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");
                if (taggedUnions) {
                    mo.put(TAGGED_UNIONS, true);
                    if (cm.discriminator != null && cm.children != null) {
                        for (CodegenModel child : cm.children) {
                            cm.imports.add(child.classname);
                        }
                    }
                    if (cm.parent != null) {
                        cm.imports.remove(cm.parent);
                    }
                }
                // Add additional filename information for imports
                mo.put("tsImports", toTsImports(cm, cm.imports));
            }
        }
        return result;
    }

    private List<Map<String, String>> toTsImports(CodegenModel cm, Set<String> imports) {
        List<Map<String, String>> tsImports = new ArrayList<>();
        for (String im : imports) {
            if (!im.equals(cm.classname)) {
                HashMap<String, String> tsImport = new HashMap<>();
                tsImport.put("classname", im);
                tsImport.put("filename", toModelFilename(im));
                tsImports.add(tsImport);
            }
        }
        return tsImports;
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return "DefaultService";
        }
        return initialCaps(name) + "Service";
    }

    @Override
    public String toApiFilename(String name) {
        if (name.length() == 0) {
            return "default.service";
        }
        return camelize(name, true) + ".service";
    }

    @Override
    public String toApiImport(String name) {
        return apiPackage() + "/" + toApiFilename(name);
    }

    @Override
    public String toModelFilename(String name) {
        return camelize(toModelName(name), true);
    }

    @Override
    public String toModelImport(String name) {
        return modelPackage() + "/" + toModelFilename(name);
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

    private String getApiFilenameFromClassname(String classname) {
        String name = classname.substring(0, classname.length() - "Service".length());
        return toApiFilename(name);
    }

    private String getModelnameFromModelFilename(String filename) {
        String name = filename.substring((modelPackage() + "/").length());
        return camelize(name);
    }

}
