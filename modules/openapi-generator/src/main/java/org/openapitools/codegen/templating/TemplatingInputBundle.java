package org.openapitools.codegen.templating;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.openapitools.codegen.CodegenSecurity;

public class TemplatingInputBundle extends BaseBundle {

    private String apiPackage;
    private OpenAPI openAPI;
    private String modelPackage;
    private String basePath;
    private String basePathWithoutHost;
    private String scheme;
    private String host;
    private String contextPath;
    private ApiInfoBundle apiInfo;
    private List<ModelBundle> models = new ArrayList<>();
    private String apiFolder;
    private List<CodegenSecurity> authMethods = new ArrayList<>();
    private ExternalDocumentation externalDocs;
    private List<Map<String, String>> pathSet = new ArrayList<>();
    private boolean hasAuthMethods;
    private boolean hasOAuthMethods;
    private List<CodegenSecurity> oauthMethods;
    private boolean hasBearerMethods;

    // hacky workaround for the templating engine to fetch truly dynamic values
    @Override
    public Object get(Object k) {
        switch ((String) k) {
            case "hasAuthMethods":
                return !authMethods.isEmpty();
        }
        return super.get(k);
    }

    @Override
    public boolean containsKey(Object k) {
        if ("hasAuthMethods".equals(k)) {
            return true;
        }
        return super.containsKey(k);
    }

    // getters and setters. Each setter puts the value in the underlying Map

    public String getApiPackage() {
        return apiPackage;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
        put("apiPackage", apiPackage);
    }


    public OpenAPI getOpenAPI() {
        return openAPI;
    }

    public void setOpenAPI(OpenAPI openAPI) {
        this.openAPI = openAPI;
        put("openAPI", openAPI);
    }


    public String getModelPackage() {
        return modelPackage;
    }

    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
        put("modelPackage", modelPackage);
    }


    public String getBasePath() {
        return basePath;
    }

    public void setBasePath(String basePath) {
        this.basePath = basePath;
        put("basePath", basePath);
    }


    public String getBasePathWithoutHost() {
        return basePathWithoutHost;
    }

    public void setBasePathWithoutHost(String basePathWithoutHost) {
        this.basePathWithoutHost = basePathWithoutHost;
        put("basePathWithoutHost", basePathWithoutHost);
    }


    public String getScheme() {
        return scheme;
    }

    public void setScheme(String scheme) {
        this.scheme = scheme;
        put("scheme", scheme);
    }

    public String getHost() {
        return host;
    }

    public void setHost(String host) {
        this.host = host;
        put("host", host);
    }


    public String getContextPath() {
        return contextPath;
    }

    public void setContextPath(String contextPath) {
        this.contextPath = contextPath;
        put("contextPath", contextPath);
    }


    public ApiInfoBundle getApiInfo() {
        return apiInfo;
    }

    public void setApiInfo(ApiInfoBundle apiInfo) {
        this.apiInfo = apiInfo;
        put("apiInfo", apiInfo);
    }


    public List<ModelBundle> getModels() {
        return models;
    }

    public void setModels(List<ModelBundle> models) {
        this.models = models;
        put("models", models);
    }


    public String getApiFolder() {
        return apiFolder;
    }

    public void setApiFolder(String apiFolder) {
        this.apiFolder = apiFolder;
        put("apiFolder", apiFolder);
    }


    public List<CodegenSecurity> getAuthMethods() {
        return authMethods;
    }

    public void setAuthMethods(List<CodegenSecurity> authMethods) {
        this.authMethods = authMethods;
        put("authMethods", authMethods);
    }


    public ExternalDocumentation getExternalDocs() {
        return externalDocs;
    }

    public void setExternalDocs(ExternalDocumentation externalDocs) {
        this.externalDocs = externalDocs;
        put("externalDocs", externalDocs);
    }


    public boolean getHasAuthMethods() {
        return hasAuthMethods;
    }

    public void setHasAuthMethods(boolean hasAuthMethods) {
        this.hasAuthMethods = hasAuthMethods;
        put("hasAuthMethods", hasAuthMethods);
    }


    public boolean getHasOAuthMethods() {
        return hasOAuthMethods;
    }

    public void setHasOAuthMethods(boolean hasOAuthMethods) {
        this.hasOAuthMethods = hasOAuthMethods;
        put("hasOAuthMethods", hasOAuthMethods);
    }


    public List<CodegenSecurity> getOauthMethods() {
        return oauthMethods;
    }

    public void setOauthMethods(List<CodegenSecurity> oauthMethods) {
        this.oauthMethods = oauthMethods;
        put("oauthMethods", oauthMethods);
    }


    public boolean getHasBearerMethods() {
        return this.hasBearerMethods;
    }

    public void setHasBearerMethods(boolean hasBearerMethods) {
        this.hasBearerMethods = hasBearerMethods;
        put("hasBearerMethods", hasBearerMethods);
    }
}
