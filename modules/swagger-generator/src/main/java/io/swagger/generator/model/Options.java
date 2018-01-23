package io.swagger.generator.model;

import java.util.ArrayList;
import java.util.List;

public class Options {

    private String lang;
    private String auth;
    private String apiPackage;
    private String modelPackage;
    private String modelNamePrefix;
    private String modelNameSuffix;
    protected List<String> systemProperties;
    private List<String> instantiationTypes;
    private List<String> typeMappings;
    private List<String> additionalProperties;
    private List<String> languageSpecificPrimitives;
    private List<String> importMappings;
    private String invokerPackage;
    private String groupId;
    private String artifactId;
    private String artifactVersion;
    private String library;
    private String gitUserId;
    private String gitRepoId;
    private String releaseNote;
    private String httpUserAgent;
    private List<String> reservedWordsMappings;
    private String ignoreFileOverride;
    private Boolean removeOperationIdPrefix;

    public Options lang(String lang) {
        this.lang = lang;
        return this;
    }

    public String getLang() {
        return lang;
    }
    public void setLang(String lang) {
        this.lang = lang;
    }

    public Options auth(String auth) {
        this.auth = auth;
        return this;
    }

    public String getAuth() {
        return auth;
    }
    public void setAuth(String auth) {
        this.auth = auth;
    }

    public Options apiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
        return this;
    }

    public String getApiPackage() {
        return apiPackage;
    }
    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public Options modelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
        return this;
    }

    public String getModelPackage() {
        return modelPackage;
    }
    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
    }

    public Options modelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
        return this;
    }

    public String getModelNamePrefix() {
        return modelNamePrefix;
    }
    public void setModelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
    }

    public Options modelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
        return this;
    }

    public String getModelNameSuffix() {
        return modelNameSuffix;
    }
    public void setModelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
    }

    public Options systemProperties(List<String> systemProperties) {
        this.systemProperties = systemProperties;
        return this;
    }

    public List<String> getSystemProperties() {
        return systemProperties;
    }

    public void setSystemProperties(List<String> systemProperties) {
        this.systemProperties = systemProperties;
    }

    public Options instantiationTypes(List<String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
        return this;
    }

    public Options addInstantiationTypesItem(String instantiationTypesItem) {

        if (this.instantiationTypes == null) {
            this.instantiationTypes = new ArrayList<String>();
        }

        this.instantiationTypes.add(instantiationTypesItem);
        return this;
    }

    public List<String> getInstantiationTypes() {
        return instantiationTypes;
    }
    public void setInstantiationTypes(List<String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
    }

    public Options typeMappings(List<String> typeMappings) {
        this.typeMappings = typeMappings;
        return this;
    }

    public Options addTypeMappingsItem(String typeMappingsItem) {

        if (this.typeMappings == null) {
            this.typeMappings = new ArrayList<String>();
        }

        this.typeMappings.add(typeMappingsItem);
        return this;
    }

    public List<String> getTypeMappings() {
        return typeMappings;
    }
    public void setTypeMappings(List<String> typeMappings) {
        this.typeMappings = typeMappings;
    }

    public Options additionalProperties(List<String> additionalProperties) {
        this.additionalProperties = additionalProperties;
        return this;
    }

    public Options addAdditionalPropertiesItem(String additionalPropertiesItem) {

        if (this.additionalProperties == null) {
            this.additionalProperties = new ArrayList<String>();
        }

        this.additionalProperties.add(additionalPropertiesItem);
        return this;
    }

    public List<String> getAdditionalProperties() {
        return additionalProperties;
    }
    public void setAdditionalProperties(List<String> additionalProperties) {
        this.additionalProperties = additionalProperties;
    }

    public Options importMappings(List<String> importMappings) {
        this.importMappings = importMappings;
        return this;
    }

    public Options addImportMappingsItem(String importMappingsItem) {

        if (this.importMappings == null) {
            this.importMappings = new ArrayList<String>();
        }

        this.importMappings.add(importMappingsItem);
        return this;
    }

    public List<String> getImportMappings() {
        return importMappings;
    }
    public void setImportMappings(List<String> importMappings) {
        this.importMappings = importMappings;
    }

    public Options invokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
        return this;
    }

    public List<String> getLanguageSpecificPrimitives() {
        return languageSpecificPrimitives;
    }

    public void setLanguageSpecificPrimitives(List<String> languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
    }

    public Options languageSpecificPrimitives(List<String>  languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
        return this;
    }

    public String getInvokerPackage() {
        return invokerPackage;
    }
    public void setInvokerPackage(String invokerPackage) {
        this.invokerPackage = invokerPackage;
    }

    public Options groupId(String groupId) {
        this.groupId = groupId;
        return this;
    }

    public String getGroupId() {
        return groupId;
    }
    public void setGroupId(String groupId) {
        this.groupId = groupId;
    }

    public Options artifactId(String artifactId) {
        this.artifactId = artifactId;
        return this;
    }

    public String getArtifactId() {
        return artifactId;
    }
    public void setArtifactId(String artifactId) {
        this.artifactId = artifactId;
    }

    public Options artifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
        return this;
    }

    public String getArtifactVersion() {
        return artifactVersion;
    }
    public void setArtifactVersion(String artifactVersion) {
        this.artifactVersion = artifactVersion;
    }

    public Options library(String library) {
        this.library = library;
        return this;
    }

    /**
     * library template (sub-template)
     * @return library
     **/
    public String getLibrary() {
        return library;
    }
    public void setLibrary(String library) {
        this.library = library;
    }

    public Options gitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
        return this;
    }

    public String getGitUserId() {
        return gitUserId;
    }
    public void setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
    }

    public Options gitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
        return this;
    }

    public String getGitRepoId() {
        return gitRepoId;
    }
    public void setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
    }

    public Options releaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
        return this;
    }

    public String getReleaseNote() {
        return releaseNote;
    }
    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    public Options httpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
        return this;
    }

    /**
     * HTTP user agent, e.g. codegen_csharp_api_client, default to ';Swagger-Codegen/{packageVersion}}/{language}';
     **/
    public String getHttpUserAgent() {
        return httpUserAgent;
    }
    public void setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
    }

    public Options reservedWordsMappings(List<String> reservedWordsMappings) {
        this.reservedWordsMappings = reservedWordsMappings;
        return this;
    }

    public Options addReservedWordsMappingsItem(String reservedWordsMappingsItem) {

        if (this.reservedWordsMappings == null) {
            this.reservedWordsMappings = new ArrayList<String>();
        }

        this.reservedWordsMappings.add(reservedWordsMappingsItem);
        return this;
    }

    public List<String> getReservedWordsMappings() {
        return reservedWordsMappings;
    }
    public void setReservedWordsMappings(List<String> reservedWordsMappings) {
        this.reservedWordsMappings = reservedWordsMappings;
    }

    public Options ignoreFileOverride(String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
        return this;
    }

    public String getIgnoreFileOverride() {
        return ignoreFileOverride;
    }
    public void setIgnoreFileOverride(String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
    }

    public Options removeOperationIdPrefix(Boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
        return this;
    }

    public Boolean getRemoveOperationIdPrefix() {
        return removeOperationIdPrefix;
    }

    public void setRemoveOperationIdPrefix(Boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
    }
}
