package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.SupportingFile;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;

import java.io.File;
import java.util.Arrays;
import java.util.HashSet;

import org.apache.commons.lang.StringUtils;

public class AndroidClientCodegen extends DefaultCodegen implements CodegenConfig {
    public static final String USE_ANDROID_MAVEN_GRADLE_PLUGIN = "useAndroidMavenGradlePlugin";
    protected String invokerPackage = "io.swagger.client";
    protected String groupId = "io.swagger";
    protected String artifactId = "swagger-android-client";
    protected String artifactVersion = "1.0.0";
    protected String projectFolder = "src/main";
    protected String sourceFolder = projectFolder + "/java";
    protected Boolean useAndroidMavenGradlePlugin = true;

    // requestPackage and authPackage are used by the "volley" template/library
    protected String requestPackage = "io.swagger.client.request";
    protected String authPackage = "io.swagger.client.auth";

    public AndroidClientCodegen() {
        super();
        outputFolder = "generated-code/android";
        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        embeddedTemplateDir = templateDir = "android";
        apiPackage = "io.swagger.client.api";
        modelPackage = "io.swagger.client.model";

        reservedWords = new HashSet<String>(
                Arrays.asList(
                    // local variable names used in API methods (endpoints)
                    "postBody", "path", "queryParams", "headerParams", "formParams",
                    "contentTypes", "contentType", "response", "builder", "httpEntity",
                    "authNames", "basePath", "apiInvoker",

                    // android reserved words
                    "abstract", "continue", "for", "new", "switch", "assert",
                    "default", "if", "package", "synchronized", "boolean", "do", "goto", "private",
                    "this", "break", "double", "implements", "protected", "throw", "byte", "else",
                    "import", "public", "throws", "case", "enum", "instanceof", "return", "transient",
                    "catch", "extends", "int", "short", "try", "char", "final", "interface", "static",
                    "void", "class", "finally", "long", "strictfp", "volatile", "const", "float",
                    "native", "super", "while")
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
                        "Object")
        );
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, "groupId for use in the generated build.gradle and pom.xml"));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, "artifactId for use in the generated build.gradle and pom.xml"));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, "artifact version for use in the generated build.gradle and pom.xml"));
        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC));
        cliOptions.add(CliOption.newBoolean(USE_ANDROID_MAVEN_GRADLE_PLUGIN, "A flag to toggle android-maven gradle plugin.")
                .defaultValue(Boolean.TRUE.toString()));

        supportedLibraries.put("<default>", "HTTP client: Apache HttpClient 4.3.6. JSON processing: Gson 2.3.1");
        supportedLibraries.put("volley", "HTTP client: Volley 1.0.19");
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
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = swaggerType;
        }
        return toModelName(type);
    }

    @Override
    public String toVarName(String name) {
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
        if (reservedWords.contains(name) || name.matches("^\\d.*")) {
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
        // model name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(name)) {
            throw new RuntimeException(name + " (reserved word) cannot be used as a model name");
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
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (reservedWords.contains(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(operationId, true);
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

        if (StringUtils.isEmpty(getLibrary())) {
            addSupportingFilesForDefault();
        } else if ("volley".equals(getLibrary())) {
            addSupportingFilesForVolley();
        }
    }

    private void addSupportingFilesForDefault() {
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
    }

    private void addSupportingFilesForVolley() {
        // supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
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

}
