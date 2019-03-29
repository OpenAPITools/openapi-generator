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
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.*;

public class TypeScriptAngularClientCodegen extends AbstractTypeScriptClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(TypeScriptAngularClientCodegen.class);

    private static String CLASS_NAME_SUFFIX_PATTERN = "^[a-zA-Z0-9]*$";
    private static String FILE_NAME_SUFFIX_PATTERN = "^[a-zA-Z0-9.-]*$";

    public static final String NPM_NAME = "npmName";
    public static final String NPM_VERSION = "npmVersion";
    public static final String NPM_REPOSITORY = "npmRepository";
    public static final String SNAPSHOT = "snapshot";
    public static final String WITH_INTERFACES = "withInterfaces";
    public static final String TAGGED_UNIONS = "taggedUnions";
    public static final String NG_VERSION = "ngVersion";
    public static final String PROVIDED_IN_ROOT = "providedInRoot";
    public static final String SERVICE_SUFFIX = "serviceSuffix";
    public static final String SERVICE_FILE_SUFFIX = "serviceFileSuffix";
    public static final String MODEL_SUFFIX = "modelSuffix";
    public static final String MODEL_FILE_SUFFIX = "modelFileSuffix";
    public static final String FILE_NAMING = "fileNaming";

    protected String npmName = null;
    protected String npmVersion = "1.0.0";
    protected String npmRepository = null;
    protected String serviceSuffix = "Service";
    protected String serviceFileSuffix = ".service";
    protected String modelSuffix = "";
    protected String modelFileSuffix = "";
    protected String fileNaming = "camelCase";

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
        this.cliOptions.add(new CliOption(NG_VERSION, "The version of Angular. Default is '7.0.0'"));
        this.cliOptions.add(new CliOption(SERVICE_SUFFIX, "The suffix of the generated service. Default is 'Service'."));
        this.cliOptions.add(new CliOption(SERVICE_FILE_SUFFIX, "The suffix of the file of the generated service (service<suffix>.ts). Default is '.service'."));
        this.cliOptions.add(new CliOption(MODEL_SUFFIX, "The suffix of the generated model. Default is ''."));
        this.cliOptions.add(new CliOption(MODEL_FILE_SUFFIX, "The suffix of the file of the generated model (model<suffix>.ts). Default is ''."));
        this.cliOptions.add(new CliOption(FILE_NAMING, "Naming convention for the output files: 'camelCase', 'kebab-case'. Default is 'camelCase'."));
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        codegenModel.additionalPropertiesType = getTypeDeclaration(ModelUtils.getAdditionalProperties(schema));
        addImport(codegenModel, codegenModel.additionalPropertiesType);
    }

    @Override
    public String getName() {
        return "typescript-angular";
    }

    @Override
    public String getHelp() {
        return "Generates a TypeScript Angular (2.x - 7.x) client library.";
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
            ngVersion = new SemVer("7.0.0");
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
            if (!additionalProperties.containsKey(PROVIDED_IN_ROOT)) {
                additionalProperties.put(PROVIDED_IN_ROOT, true);
            } else {
                additionalProperties.put(PROVIDED_IN_ROOT, Boolean.valueOf(
                        (String) additionalProperties.get(PROVIDED_IN_ROOT)));
            }
        } else {
            additionalProperties.put(PROVIDED_IN_ROOT, false);
        }

        additionalProperties.put(NG_VERSION, ngVersion);
        additionalProperties.put("injectionToken", ngVersion.atLeast("4.0.0") ? "InjectionToken" : "OpaqueToken");
        additionalProperties.put("injectionTokenTyped", ngVersion.atLeast("4.0.0"));
        additionalProperties.put("useHttpClient", ngVersion.atLeast("4.3.0"));
        additionalProperties.put("useRxJS6", ngVersion.atLeast("6.0.0"));
        if (!ngVersion.atLeast("4.3.0")) {
            supportingFiles.add(new SupportingFile("rxjs-operators.mustache", getIndexDirectory(), "rxjs-operators.ts"));
        }
        if (additionalProperties.containsKey(SERVICE_SUFFIX)) {
            serviceSuffix = additionalProperties.get(SERVICE_SUFFIX).toString();
            validateClassSuffixArgument("Service", serviceSuffix);
        }
        if (additionalProperties.containsKey(SERVICE_FILE_SUFFIX)) {
            serviceFileSuffix = additionalProperties.get(SERVICE_FILE_SUFFIX).toString();
            validateFileSuffixArgument("Service", serviceFileSuffix);
        }
        if (additionalProperties.containsKey(MODEL_SUFFIX)) {
            modelSuffix = additionalProperties.get(MODEL_SUFFIX).toString();
            validateClassSuffixArgument("Model", modelSuffix);
        }
        if (additionalProperties.containsKey(MODEL_FILE_SUFFIX)) {
            modelFileSuffix = additionalProperties.get(MODEL_FILE_SUFFIX).toString();
            validateFileSuffixArgument("Model", modelFileSuffix);
        }
        if (additionalProperties.containsKey(FILE_NAMING)) {
            this.setFileNaming(additionalProperties.get(FILE_NAMING).toString());
        }
    }

    private void addNpmPackageGeneration(SemVer ngVersion) {

        if (additionalProperties.containsKey(NPM_NAME)) {
            this.setNpmName(additionalProperties.get(NPM_NAME).toString());
        }

        if (additionalProperties.containsKey(NPM_VERSION)) {
            this.setNpmVersion(additionalProperties.get(NPM_VERSION).toString());
        } else if (this.getVersionFromApi() != null) {
            this.setNpmVersion(this.getVersionFromApi());
        }

        if (additionalProperties.containsKey(SNAPSHOT) && Boolean.valueOf(additionalProperties.get(SNAPSHOT).toString())) {
            if (npmVersion.toUpperCase(Locale.ROOT).matches("^.*-SNAPSHOT$")) {
                this.setNpmVersion(npmVersion + "." + SNAPSHOT_SUFFIX_FORMAT.format(new Date()));
            }
            else {
                this.setNpmVersion(npmVersion + "-SNAPSHOT." + SNAPSHOT_SUFFIX_FORMAT.format(new Date()));
            }
        }
        additionalProperties.put(NPM_VERSION, npmVersion);

        if (additionalProperties.containsKey(NPM_REPOSITORY)) {
            this.setNpmRepository(additionalProperties.get(NPM_REPOSITORY).toString());
        }

        // Set the typescript version compatible to the Angular version
        if (ngVersion.atLeast("7.0.0")) {
            // Angular v7 requires typescript ">=3.1.1 <3.2.0"
            additionalProperties.put("tsVersion", ">=3.1.1 <3.2.0");
        } else if (ngVersion.atLeast("6.0.0")) {
            additionalProperties.put("tsVersion", ">=2.7.2 and <2.10.0");
        } else if (ngVersion.atLeast("5.0.0")) {
            additionalProperties.put("tsVersion", ">=2.1.5 <2.7.0");
        } else {
            // Angular v2-v4 requires typescript ">=2.1.5 <2.8"
            additionalProperties.put("tsVersion", ">=2.1.5 <2.8.0");
        }

        // Set the rxJS version compatible to the Angular version
        if (ngVersion.atLeast("7.0.0")) {
            additionalProperties.put("rxjsVersion", "6.3.0");
        } else if (ngVersion.atLeast("6.0.0")) {
            additionalProperties.put("rxjsVersion", "6.1.0");
        } else {
            // Angular prior to v6
            additionalProperties.put("rxjsVersion", "5.4.0");
        }

        if (!ngVersion.atLeast("4.3.0")) {
            supportingFiles.add(new SupportingFile("rxjs-operators.mustache", getIndexDirectory(), "rxjs-operators.ts"));
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

        // Specific ng-packagr configuration
        if (ngVersion.atLeast("7.0.0")) {
            // compatible versions with typescript version
            additionalProperties.put("ngPackagrVersion", "4.4.5");
            additionalProperties.put("tsickleVersion", "0.34.0");
        } else if (ngVersion.atLeast("6.0.0")) {
            // compatible versions with typescript version
            additionalProperties.put("ngPackagrVersion", "3.0.6");
            additionalProperties.put("tsickleVersion", "0.32.1");
        } else if (ngVersion.atLeast("5.0.0")) {
            // compatible versions with typescript version
            additionalProperties.put("ngPackagrVersion", "2.4.5");
            additionalProperties.put("tsickleVersion", "0.27.5");
        } else {
            // Angular versions prior to v5
            additionalProperties.put("ngPackagrVersion", "1.6.0");
        }

        // set zone.js version
        if (ngVersion.atLeast("5.0.0")) {
            // compatible versions to Angular 5+
            additionalProperties.put("zonejsVersion", "0.8.26");
        } else {
            // Angular versions prior to v5
            additionalProperties.put("zonejsVersion", "0.7.6");
        }

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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> operations, List<Object> allModels) {
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
            im.put("classname", im.get("classname"));
        }

        return operations;
    }

    /**
     * Finds and returns a path parameter of an operation by its name
     *
     * @param operation the operation
     * @param parameterName the name of the parameter
     * @return param
     */
    private CodegenParameter findPathParameterByName(CodegenOperation operation, String parameterName) {
        for (CodegenParameter param : operation.pathParams) {
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
                // TVG: This is used as class name in the import statements of the model file
                tsImport.put("classname", im);
                tsImport.put("filename", toModelFilename(removeModelSuffixIfNecessary(im)));
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
        return camelize(name) + serviceSuffix;
    }

    @Override
    public String toApiFilename(String name) {
        if (name.length() == 0) {
            return "default.service";
        }
        return this.convertUsingFileNamingConvention(name) + serviceFileSuffix;
    }

    @Override
    public String toApiImport(String name) {
        return apiPackage() + "/" + toApiFilename(name);
    }

    @Override
    public String toModelFilename(String name) {
        return this.convertUsingFileNamingConvention(this.sanitizeName(name)) + modelFileSuffix;
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
        String name = classname.substring(0, classname.length() - serviceSuffix.length());
        return toApiFilename(name);
    }

    /*
    private String getModelnameFromModelFilename(String filename) {
        String name = filename.substring((modelPackage() + "/").length());
        // Remove the file suffix and add the class suffix.
        // This is needed because the model file suffix might not be the same as
        // the model suffix.
        if (modelFileSuffix.length() > 0) {
            name = name.substring(0, name.length() - modelFileSuffix.length());
        }
        return camelize(name) + modelSuffix;
    }
    */

    @Override
    public String toModelName(String name) {
        String modelName = super.toModelName(name);
        if (modelSuffix.length() == 0 || modelName.endsWith(modelSuffix)) {
            return modelName;
        }
        return modelName + modelSuffix;
    }

    private String removeModelSuffixIfNecessary(String name) {
        if (modelSuffix.length() == 0 || !name.endsWith(modelSuffix)) {
            return name;
        }
        return name.substring(0, name.length() - modelSuffix.length());
    }

    /**
     * Validates that the given string value only contains '-', '.' and alpha numeric characters.
     * Throws an IllegalArgumentException, if the string contains any other characters.
     *
     * @param argument The name of the argument being validated. This is only used for displaying an error message.
     * @param value    The value that is being validated.
     */
    private void validateFileSuffixArgument(String argument, String value) {
        if (!value.matches(FILE_NAME_SUFFIX_PATTERN)) {
            throw new IllegalArgumentException(
                    String.format(Locale.ROOT, "%s file suffix only allows '.', '-' and alphanumeric characters.", argument)
            );
        }
    }

    /**
     * Validates that the given string value only contains alpha numeric characters.
     * Throws an IllegalArgumentException, if the string contains any other characters.
     *
     * @param argument The name of the argument being validated. This is only used for displaying an error message.
     * @param value    The value that is being validated.
     */
    private void validateClassSuffixArgument(String argument, String value) {
        if (!value.matches(CLASS_NAME_SUFFIX_PATTERN)) {
            throw new IllegalArgumentException(
                    String.format(Locale.ROOT, "%s class suffix only allows alphanumeric characters.", argument)
            );
        }
    }

    /**
     * Set the file naming type.
     *
     * @param fileNaming the file naming to use
     */
    private void setFileNaming(String fileNaming) {
        if ("camelCase".equals(fileNaming) || "kebab-case".equals(fileNaming)) {
            this.fileNaming = fileNaming;
        } else {
            throw new IllegalArgumentException("Invalid file naming '" +
                    fileNaming + "'. Must be 'camelCase' or 'kebab-case'");
        }
    }

    /**
     * Converts the original name according to the current <code>fileNaming</code> strategy.
     *
     * @param originalName the original name to transform
     * @return the transformed name
     */
    private String convertUsingFileNamingConvention(String originalName) {
        String name = this.removeModelSuffixIfNecessary(originalName);
        if ("kebab-case".equals(fileNaming)) {
            name = dashize(underscore(name));
        } else {
            name = camelize(name, true);
        }
        return name;
    }

    /**
     * Returns version from OpenAPI info.
     *
     * @return version
     */
    private String getVersionFromApi() {
        if (this.openAPI != null && this.openAPI.getInfo() != null) {
            return this.openAPI.getInfo().getVersion();
        } else {
            return null;
        }
    }
}
