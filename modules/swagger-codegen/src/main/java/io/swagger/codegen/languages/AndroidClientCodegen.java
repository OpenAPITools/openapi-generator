package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenParameter;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AndroidClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(AndroidClientCodegen.class);
    public static final String USE_ANDROID_MAVEN_GRADLE_PLUGIN = "useAndroidMavenGradlePlugin";
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-android-client";
    protected String artifactVersion = "1.0.0";
    protected String projectFolder = "src/main";
    protected String sourceFolder = projectFolder + "/java";
    protected Boolean useAndroidMavenGradlePlugin = true;
    protected Boolean serializableModel = false;

    // requestPackage and authPackage are used by the "volley" template/library
    protected String requestPackage = "io.swagger.client.request";
    protected String authPackage = "io.swagger.client.auth";
    protected String gradleWrapperPackage = "gradle.wrapper";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public AndroidClientCodegen() {
        super();
        outputFolder = "generated-code/android";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        embeddedTemplateDir = templateDir = "android";
        apiPackage = "io.swagger.client.api";
        modelPackage = "io.swagger.client.model";

        setReservedWordsLowerCase(
                Arrays.asList(
                    // local variable names used in API methods (endpoints)
                    "localVarPostBody", "localVarPath", "localVarQueryParams", "localVarHeaderParams",
                    "localVarFormParams", "localVarContentTypes", "localVarContentType",
                    "localVarResponse", "localVarBuilder", "authNames", "basePath", "apiInvoker",

                    // due to namespace collusion
                    "Object",

                    // android reserved words
                    "abstract", "continue", "for", "new", "switch", "assert",
                    "default", "if", "package", "synchronized", "boolean", "do", "goto", "private",
                    "this", "break", "double", "implements", "protected", "throw", "byte", "else",
                    "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
                    "catch", "extends", "int", "short", "try", "char", "final", "interface", "static",
                    "void", "class", "finally", "long", "strictfp", "volatile", "const", "float",
                    "native", "super", "while", "null")
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "byte[]",
                        "Object")
        );
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");
        typeMapping.put("date", "Date");
        typeMapping.put("file", "File");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, "groupId for use in the generated build.gradle and pom.xml"));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, "artifactId for use in the generated build.gradle and pom.xml"));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, "artifact version for use in the generated build.gradle and pom.xml"));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
        cliOptions.add(CliOption.newBoolean(USE_ANDROID_MAVEN_GRADLE_PLUGIN, "A flag to toggle android-maven gradle plugin.")
                .defaultValue(Boolean.TRUE.toString()));

        cliOptions.add(CliOption.newBoolean(CodegenConstants.SERIALIZABLE_MODEL, CodegenConstants.SERIALIZABLE_MODEL_DESC));

        supportedLibraries.put("volley", "HTTP client: Volley 1.0.19 (default)");
        supportedLibraries.put("httpclient", "HTTP client: Apache HttpClient 4.3.6. JSON processing: Gson 2.3.1. IMPORTANT: Android client using HttpClient is not actively maintained and will be depecreated in the next major release.");
        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setEnum(supportedLibraries);
        cliOptions.add(library);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "android";
    }

    @Override
    public String getHelp() {
        return "Generates an Android client library.";
    }

    @Override
    public String escapeReservedWord(String name) {           
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace( '/', File.separatorChar );
    }

    @Override
    public String modelDocFileFolder() {
        return ( outputFolder + "/" + modelDocPath ).replace( '/', File.separatorChar );
    }

    @Override
    public String toApiDocFilename( String name ) {
        return toApiName( name );
    }

    @Override
    public String toModelDocFilename( String name ) {
        return toModelName( name );
    }

    @Override
    public String getTypeDeclaration(Property p) {
        if (p instanceof ArrayProperty) {
            ArrayProperty ap = (ArrayProperty) p;
            Property inner = ap.getItems();
            return getSwaggerType(p) + "<" + getTypeDeclaration(inner) + ">";
        } else if (p instanceof MapProperty) {
            MapProperty mp = (MapProperty) p;
            Property inner = mp.getAdditionalProperties();

            return getSwaggerType(p) + "<String, " + getTypeDeclaration(inner) + ">";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSwaggerType(Property p) {
        String swaggerType = super.getSwaggerType(p);
        String type = null;
        if (typeMapping.containsKey(swaggerType)) {
            type = typeMapping.get(swaggerType);
            if (languageSpecificPrimitives.contains(type) || type.indexOf(".") >= 0 ||
                type.equals("Map") || type.equals("List") ||
                type.equals("File") || type.equals("Date")) {
                return type;
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        // camelize (lower first character) the variable name
        // pet_id => petId
        name = camelize(name, true);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
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
        // add prefix, suffix if needed
        if (!StringUtils.isEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        name = camelize(sanitizeName(name));

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            String modelName = "Model" + name;
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            String modelName = "Model" + name; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return name;
    }

    @Override
    public String toModelFilename(String name) {
        // should be the same as the model name
        return toModelName(name);
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

        if ("String".equals(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "\"" + escapeText(example) + "\"";
        } else if ("Integer".equals(type) || "Short".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Long".equals(type)) {
            if (example == null) {
                example = "56";
            }
            example = example + "L";
        } else if ("Float".equals(type)) {
            if (example == null) {
                example = "3.4";
            }
            example = example + "F";
        } else if ("Double".equals(type)) {
            example = "3.4";
            example = example + "D";
        } else if ("Boolean".equals(type)) {
            if (example == null) {
                example = "true";
            }
        } else if ("File".equals(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "new File(\"" + escapeText(example) + "\")";
        } else if ("Date".equals(type)) {
            example = "new Date()";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. User
            example = "new " + type + "()";
        }

        if (example == null) {
            example = "null";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "Arrays.asList(" + example + ")";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "new HashMap()";
        }

        p.example = example;
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        operationId = camelize(sanitizeName(operationId), true);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            String newOperationId = camelize("call_" + operationId, true);
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + newOperationId);
            return newOperationId;
        }

        return operationId;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            this.setInvokerPackage((String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE));
        } else {
            //not set, use default to be passed to template
            additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        }

        if (additionalProperties.containsKey(CodegenConstants.GROUP_ID)) {
            this.setGroupId((String) additionalProperties.get(CodegenConstants.GROUP_ID));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_ID)) {
            this.setArtifactId((String) additionalProperties.get(CodegenConstants.ARTIFACT_ID));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        }

        if (additionalProperties.containsKey(CodegenConstants.ARTIFACT_VERSION)) {
            this.setArtifactVersion((String) additionalProperties.get(CodegenConstants.ARTIFACT_VERSION));
        } else {
            //not set, use to be passed to template
            additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        }

        if (additionalProperties.containsKey(USE_ANDROID_MAVEN_GRADLE_PLUGIN)) {
            this.setUseAndroidMavenGradlePlugin(Boolean.valueOf((String) additionalProperties
                    .get(USE_ANDROID_MAVEN_GRADLE_PLUGIN)));
        } else {
            additionalProperties.put(USE_ANDROID_MAVEN_GRADLE_PLUGIN, useAndroidMavenGradlePlugin);
        }

        if (additionalProperties.containsKey(CodegenConstants.LIBRARY)) {
            this.setLibrary((String) additionalProperties.get(CodegenConstants.LIBRARY));
        }

        if (additionalProperties.containsKey(CodegenConstants.SERIALIZABLE_MODEL)) {
            this.setSerializableModel(Boolean.valueOf(additionalProperties.get(CodegenConstants.SERIALIZABLE_MODEL).toString()));
        }

        // need to put back serializableModel (boolean) into additionalProperties as value in additionalProperties is string
        additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, serializableModel);

        //make api and model doc path available in mustache template
        additionalProperties.put( "apiDocPath", apiDocPath );
        additionalProperties.put( "modelDocPath", modelDocPath );

        if (StringUtils.isEmpty(getLibrary())) {
            setLibrary("volley"); // set volley as the default library
        }

        // determine which file (mustache) to add based on library
        if ("volley".equals(getLibrary())) {
            addSupportingFilesForVolley();
        } else if ("httpclient".equals(getLibrary())) {
            addSupportingFilesForHttpClient();
        } else {
            throw new IllegalArgumentException("Invalid 'library' option specified: '" + getLibrary() + "'. Must be 'httpclient' or 'volley' (default)"); 
        }

    }

    private void addSupportingFilesForHttpClient() {
        // documentation files
        modelDocTemplateFiles.put( "model_doc.mustache", ".md" );
        apiDocTemplateFiles.put( "api_doc.mustache", ".md" );
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("build.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        supportingFiles.add(new SupportingFile("apiInvoker.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "ApiInvoker.java"));
        supportingFiles.add(new SupportingFile("httpPatch.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "HttpPatch.java"));
        supportingFiles.add(new SupportingFile("jsonUtil.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "JsonUtil.java"));
        supportingFiles.add(new SupportingFile("apiException.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "ApiException.java"));
        supportingFiles.add(new SupportingFile("Pair.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "Pair.java"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        // gradle wrapper files
        supportingFiles.add(new SupportingFile( "gradlew.mustache", "", "gradlew" ));
        supportingFiles.add(new SupportingFile( "gradlew.bat.mustache", "", "gradlew.bat" ));
        supportingFiles.add(new SupportingFile( "gradle-wrapper.properties.mustache", 
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.properties" ));
        supportingFiles.add(new SupportingFile( "gradle-wrapper.jar", 
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.jar" ));

    }

    private void addSupportingFilesForVolley() {
        // documentation files
        modelDocTemplateFiles.put( "model_doc.mustache", ".md" );
        apiDocTemplateFiles.put( "api_doc.mustache", ".md" );
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        // supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("build.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        supportingFiles.add(new SupportingFile("apiInvoker.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "ApiInvoker.java"));
        supportingFiles.add(new SupportingFile("jsonUtil.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "JsonUtil.java"));
        supportingFiles.add(new SupportingFile("apiException.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "ApiException.java"));
        supportingFiles.add(new SupportingFile("Pair.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", File.separator), "Pair.java"));
        supportingFiles.add(new SupportingFile("request/getrequest.mustache",
                (sourceFolder + File.separator + requestPackage).replace(".", File.separator), "GetRequest.java"));
        supportingFiles.add(new SupportingFile("request/postrequest.mustache",
                (sourceFolder + File.separator + requestPackage).replace(".", File.separator), "PostRequest.java"));
        supportingFiles.add(new SupportingFile("request/putrequest.mustache",
                (sourceFolder + File.separator + requestPackage).replace(".", File.separator), "PutRequest.java"));
        supportingFiles.add(new SupportingFile("request/deleterequest.mustache",
                (sourceFolder + File.separator + requestPackage).replace(".", File.separator), "DeleteRequest.java"));
        supportingFiles.add(new SupportingFile("request/patchrequest.mustache",
                (sourceFolder + File.separator + requestPackage).replace(".", File.separator), "PatchRequest.java"));
        supportingFiles.add(new SupportingFile("auth/apikeyauth.mustache",
                (sourceFolder + File.separator + authPackage).replace(".", File.separator), "ApiKeyAuth.java"));
        supportingFiles.add(new SupportingFile("auth/httpbasicauth.mustache",
                (sourceFolder + File.separator + authPackage).replace(".", File.separator), "HttpBasicAuth.java"));
        supportingFiles.add(new SupportingFile("auth/authentication.mustache",
                (sourceFolder + File.separator + authPackage).replace(".", File.separator), "Authentication.java"));

        // gradle wrapper files
        supportingFiles.add(new SupportingFile( "gradlew.mustache", "", "gradlew" ));
        supportingFiles.add(new SupportingFile( "gradlew.bat.mustache", "", "gradlew.bat" ));
        supportingFiles.add(new SupportingFile( "gradle-wrapper.properties.mustache", 
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.properties" ));
        supportingFiles.add(new SupportingFile( "gradle-wrapper.jar", 
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.jar" ));
    }

    public Boolean getUseAndroidMavenGradlePlugin() {
        return useAndroidMavenGradlePlugin;
    }

    public void setUseAndroidMavenGradlePlugin(Boolean useAndroidMavenGradlePlugin) {
        this.useAndroidMavenGradlePlugin = useAndroidMavenGradlePlugin;
    }

    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setSerializableModel(Boolean serializableModel) {
        this.serializableModel = serializableModel;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

}
