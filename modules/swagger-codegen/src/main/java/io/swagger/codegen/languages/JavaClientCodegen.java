package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.codegen.languages.features.BeanValidationFeatures;

import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class JavaClientCodegen extends AbstractJavaCodegen implements BeanValidationFeatures {
    @SuppressWarnings("hiding")
    private static final Logger LOGGER = LoggerFactory.getLogger(JavaClientCodegen.class);

    public static final String USE_RX_JAVA = "useRxJava";
    public static final String PARCELABLE_MODEL = "parcelableModel";

    public static final String RETROFIT_1 = "retrofit";
    public static final String RETROFIT_2 = "retrofit2";

    protected String gradleWrapperPackage = "gradle.wrapper";
    protected boolean useRxJava = false;
    protected boolean parcelableModel = false;
    protected boolean useBeanValidation = false;

    public JavaClientCodegen() {
        super();
        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = templateDir = "Java";
        invokerPackage = "io.swagger.client";
        artifactId = "swagger-java-client";
        apiPackage = "io.swagger.client.api";
        modelPackage = "io.swagger.client.model";

        cliOptions.add(CliOption.newBoolean(USE_RX_JAVA, "Whether to use the RxJava adapter with the retrofit2 library."));
        cliOptions.add(CliOption.newBoolean(PARCELABLE_MODEL, "Whether to generate models for Android that implement Parcelable with the okhttp-gson library."));
        cliOptions.add(CliOption.newBoolean(SUPPORT_JAVA6, "Whether to support Java6 with the Jersey1 library."));
        cliOptions.add(CliOption.newBoolean(USE_BEANVALIDATION, "Use BeanValidation API annotations"));

        supportedLibraries.put("jersey1", "HTTP client: Jersey client 1.19.1. JSON processing: Jackson 2.7.0. Enable Java6 support using '-DsupportJava6=true'.");
        supportedLibraries.put("feign", "HTTP client: Netflix Feign 8.16.0. JSON processing: Jackson 2.7.0");
        supportedLibraries.put("jersey2", "HTTP client: Jersey client 2.22.2. JSON processing: Jackson 2.7.0");
        supportedLibraries.put("okhttp-gson", "HTTP client: OkHttp 2.7.5. JSON processing: Gson 2.6.2. Enable Parcelable modles on Android using '-DparcelableModel=true'");
        supportedLibraries.put(RETROFIT_1, "HTTP client: OkHttp 2.7.5. JSON processing: Gson 2.3.1 (Retrofit 1.9.0). IMPORTANT NOTE: retrofit1.x is no longer actively maintained so please upgrade to 'retrofit2' instead.");
        supportedLibraries.put(RETROFIT_2, "HTTP client: OkHttp 3.2.0. JSON processing: Gson 2.6.1 (Retrofit 2.0.2). Enable the RxJava adapter using '-DuseRxJava=true'. (RxJava 1.1.3)");

        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        libraryOption.setEnum(supportedLibraries);
        // set okhttp-gson as the default
        libraryOption.setDefault("okhttp-gson");
        cliOptions.add(libraryOption);
        setLibrary("okhttp-gson");

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "java";
    }

    @Override
    public String getHelp() {
        return "Generates a Java client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(USE_RX_JAVA)) {
            this.setUseRxJava(Boolean.valueOf(additionalProperties.get(USE_RX_JAVA).toString()));
        }
        if (additionalProperties.containsKey(PARCELABLE_MODEL)) {
            this.setParcelableModel(Boolean.valueOf(additionalProperties.get(PARCELABLE_MODEL).toString()));
        }
        // put the boolean value back to PARCELABLE_MODEL in additionalProperties
        additionalProperties.put(PARCELABLE_MODEL, parcelableModel);

        if (additionalProperties.containsKey(USE_BEANVALIDATION)) {
            boolean useBeanValidationProp = Boolean.valueOf(additionalProperties.get(USE_BEANVALIDATION).toString());
            this.setUseBeanValidation(useBeanValidationProp);

            // write back as boolean
            additionalProperties.put(USE_BEANVALIDATION, useBeanValidationProp);
        }

        final String invokerFolder = (sourceFolder + '/' + invokerPackage).replace(".", "/");
        final String authFolder = (sourceFolder + '/' + invokerPackage + ".auth").replace(".", "/");

        //Common files
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
        writeOptional(outputFolder, new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        writeOptional(outputFolder, new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        writeOptional(outputFolder, new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        writeOptional(outputFolder, new SupportingFile("gradle.properties.mustache", "", "gradle.properties"));
        writeOptional(outputFolder, new SupportingFile("manifest.mustache", projectFolder, "AndroidManifest.xml"));
        supportingFiles.add(new SupportingFile("travis.mustache", "", ".travis.yml"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache", invokerFolder, "ApiClient.java"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache", invokerFolder, "StringUtil.java"));
        supportingFiles.add(new SupportingFile("auth/HttpBasicAuth.mustache", authFolder, "HttpBasicAuth.java"));
        supportingFiles.add(new SupportingFile("auth/ApiKeyAuth.mustache", authFolder, "ApiKeyAuth.java"));
        supportingFiles.add(new SupportingFile("auth/OAuth.mustache", authFolder, "OAuth.java"));
        supportingFiles.add(new SupportingFile("auth/OAuthFlow.mustache", authFolder, "OAuthFlow.java"));
        supportingFiles.add(new SupportingFile( "gradlew.mustache", "", "gradlew") );
        supportingFiles.add(new SupportingFile( "gradlew.bat.mustache", "", "gradlew.bat") );
        supportingFiles.add(new SupportingFile( "gradle-wrapper.properties.mustache",
                gradleWrapperPackage.replace( ".", File.separator ), "gradle-wrapper.properties") );
        supportingFiles.add(new SupportingFile( "gradle-wrapper.jar",
                gradleWrapperPackage.replace( ".", File.separator ), "gradle-wrapper.jar") );
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));

        //TODO: add doc to retrofit1 and feign
        if ( "feign".equals(getLibrary()) || "retrofit".equals(getLibrary()) ){
            modelDocTemplateFiles.remove("model_doc.mustache");
            apiDocTemplateFiles.remove("api_doc.mustache");
        }

        if (!("feign".equals(getLibrary()) || usesAnyRetrofitLibrary())) {
            supportingFiles.add(new SupportingFile("apiException.mustache", invokerFolder, "ApiException.java"));
            supportingFiles.add(new SupportingFile("Configuration.mustache", invokerFolder, "Configuration.java"));
            supportingFiles.add(new SupportingFile("Pair.mustache", invokerFolder, "Pair.java"));
            supportingFiles.add(new SupportingFile("auth/Authentication.mustache", authFolder, "Authentication.java"));
        }

        if ("feign".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("FormAwareEncoder.mustache", invokerFolder, "FormAwareEncoder.java"));
            additionalProperties.put("jackson", "true");
        } else if ("okhttp-gson".equals(getLibrary()) || StringUtils.isEmpty(getLibrary())) {
            // the "okhttp-gson" library template requires "ApiCallback.mustache" for async call
            supportingFiles.add(new SupportingFile("ApiCallback.mustache", invokerFolder, "ApiCallback.java"));
            supportingFiles.add(new SupportingFile("ApiResponse.mustache", invokerFolder, "ApiResponse.java"));
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            supportingFiles.add(new SupportingFile("ProgressRequestBody.mustache", invokerFolder, "ProgressRequestBody.java"));
            supportingFiles.add(new SupportingFile("ProgressResponseBody.mustache", invokerFolder, "ProgressResponseBody.java"));
            additionalProperties.put("gson", "true");
        } else if (usesAnyRetrofitLibrary()) {
            supportingFiles.add(new SupportingFile("auth/OAuthOkHttpClient.mustache", authFolder, "OAuthOkHttpClient.java"));
            supportingFiles.add(new SupportingFile("CollectionFormats.mustache", invokerFolder, "CollectionFormats.java"));
            additionalProperties.put("gson", "true");
        } else if("jersey2".equals(getLibrary())) {
            supportingFiles.add(new SupportingFile("JSON.mustache", invokerFolder, "JSON.java"));
            additionalProperties.put("jackson", "true");
        } else if("jersey1".equals(getLibrary())) {
            additionalProperties.put("jackson", "true");
        } else {
            LOGGER.error("Unknown library option (-l/--library): " + getLibrary());
        }

        if (additionalProperties.containsKey("jackson") ) {
            supportingFiles.add(new SupportingFile("RFC3339DateFormat.mustache", invokerFolder, "RFC3339DateFormat.java"));
        }
    }

    private boolean usesAnyRetrofitLibrary() {
        return getLibrary() != null && getLibrary().contains(RETROFIT_1);
    }

    private boolean usesRetrofit2Library() {
        return getLibrary() != null && getLibrary().contains(RETROFIT_2);
    }

    @Override
    public Map<String, Object> postProcessOperations(Map<String, Object> objs) {
        super.postProcessOperations(objs);
        if(usesAnyRetrofitLibrary()) {
            Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
            if (operations != null) {
                List<CodegenOperation> ops = (List<CodegenOperation>) operations.get("operation");
                for (CodegenOperation operation : ops) {
                    if (operation.hasConsumes == Boolean.TRUE) {
                        Map<String, String> firstType = operation.consumes.get(0);
                        if (firstType != null) {
                            if ("multipart/form-data".equals(firstType.get("mediaType"))) {
                                operation.isMultipart = Boolean.TRUE;
                            }
                        }
                    }
                    if (operation.returnType == null) {
                        operation.returnType = "Void";
                    }
                    if (usesRetrofit2Library() && StringUtils.isNotEmpty(operation.path) && operation.path.startsWith("/"))
                    	operation.path = operation.path.substring(1);
                }
            }
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if(!BooleanUtils.toBoolean(model.isEnum)) {
            //final String lib = getLibrary();
            //Needed imports for Jackson based libraries
            if(additionalProperties.containsKey("jackson")) {
                model.imports.add("JsonProperty");
            }
            if(additionalProperties.containsKey("gson")) {
                model.imports.add("SerializedName");
            }
        } else { // enum class
            //Needed imports for Jackson's JsonCreator
            if(additionalProperties.containsKey("jackson")) {
                model.imports.add("JsonCreator");
            }
        }
    }

    @Override
    public Map<String, Object> postProcessModelsEnum(Map<String, Object> objs) {
        objs = super.postProcessModelsEnum(objs);
        //Needed import for Gson based libraries
        if (additionalProperties.containsKey("gson")) {
            List<Map<String, String>> imports = (List<Map<String, String>>)objs.get("imports");
            List<Object> models = (List<Object>) objs.get("models");
            for (Object _mo : models) {
                Map<String, Object> mo = (Map<String, Object>) _mo;
                CodegenModel cm = (CodegenModel) mo.get("model");
                // for enum model
                if (Boolean.TRUE.equals(cm.isEnum) && cm.allowableValues != null) {
                    cm.imports.add(importMapping.get("SerializedName"));
                    Map<String, String> item = new HashMap<String, String>();
                    item.put("import", importMapping.get("SerializedName"));
                    imports.add(item);
                }
            }
        }
        return objs;
    }

    public void setUseRxJava(boolean useRxJava) {
        this.useRxJava = useRxJava;
    }

    public void setParcelableModel(boolean parcelableModel) {
        this.parcelableModel = parcelableModel;
    }

    public void setUseBeanValidation(boolean useBeanValidation) {
        this.useBeanValidation = useBeanValidation;
    }

}
