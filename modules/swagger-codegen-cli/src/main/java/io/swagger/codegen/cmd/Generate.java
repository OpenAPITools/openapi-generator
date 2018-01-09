package io.swagger.codegen.cmd;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.CLIHelper;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.parser.util.RemoteUrl;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static io.swagger.codegen.CLIHelper.isValidJson;
import static io.swagger.codegen.CLIHelper.isValidURL;
import static io.swagger.codegen.CLIHelper.isValidYaml;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyAdditionalPropertiesKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyImportMappingsKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyInstantiationTypesKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyLanguageSpecificPrimitivesCsvList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyReservedWordsMappingsKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applySystemPropertiesKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyTypeMappingsKvpList;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

/**
 * User: lanwen Date: 24.03.15 Time: 20:22
 */

public class Generate implements Runnable {

    public static final Logger LOG = LoggerFactory.getLogger(Generate.class);

    protected Boolean verbose;
    protected String lang;
    protected String output = "";
    protected String spec;
    protected String templateDir;
    protected String auth;
    protected List<String> systemProperties = new ArrayList<>();
    protected String configFile;
    protected Boolean skipOverwrite;
    protected String apiPackage;
    protected String modelPackage;
    protected String modelNamePrefix;
    protected String modelNameSuffix;
    protected List<String> instantiationTypes = new ArrayList<>();
    protected List<String> typeMappings = new ArrayList<>();
    protected List<String> additionalProperties = new ArrayList<>();
    protected List<String> languageSpecificPrimitives = new ArrayList<>();
    protected List<String> importMappings = new ArrayList<>();
    protected String invokerPackage;
    protected String groupId;
    protected String artifactId;
    protected String artifactVersion;
    protected String library;
    protected String gitUserId;
    protected String gitRepoId;
    protected String releaseNote;
    protected String httpUserAgent;
    protected List<String> reservedWordsMappings = new ArrayList<>();
    protected String ignoreFileOverride;
    protected Boolean removeOperationIdPrefix;
    private String url;

    public void setVerbose(Boolean verbose) {
        this.verbose = verbose;
    }

    public void setLang(String lang) {
        this.lang = lang;
    }

    public void setOutput(String output) {
        this.output = output;
    }

    public void setSpec(String spec) {
        this.spec = spec;
    }

    public void setTemplateDir(String templateDir) {
        this.templateDir = templateDir;
    }

    public void setAuth(String auth) {
        this.auth = auth;
    }

    public void setSystemProperties(List<String> systemProperties) {
        this.systemProperties = systemProperties;
    }

    public void setConfigFile(String configFile) {
        this.configFile = configFile;
    }

    public void setSkipOverwrite(Boolean skipOverwrite) {
        this.skipOverwrite = skipOverwrite;
    }

    public void setApiPackage(String apiPackage) {
        this.apiPackage = apiPackage;
    }

    public void setModelPackage(String modelPackage) {
        this.modelPackage = modelPackage;
    }

    public void setModelNamePrefix(String modelNamePrefix) {
        this.modelNamePrefix = modelNamePrefix;
    }

    public void setModelNameSuffix(String modelNameSuffix) {
        this.modelNameSuffix = modelNameSuffix;
    }

    public void setInstantiationTypes(List<String> instantiationTypes) {
        this.instantiationTypes = instantiationTypes;
    }

    public void setTypeMappings(List<String> typeMappings) {
        this.typeMappings = typeMappings;
    }

    public void setAdditionalProperties(List<String> additionalProperties) {
        this.additionalProperties = additionalProperties;
    }

    public void setLanguageSpecificPrimitives(List<String> languageSpecificPrimitives) {
        this.languageSpecificPrimitives = languageSpecificPrimitives;
    }

    public void setImportMappings(List<String> importMappings) {
        this.importMappings = importMappings;
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

    public void setLibrary(String library) {
        this.library = library;
    }

    public void setGitUserId(String gitUserId) {
        this.gitUserId = gitUserId;
    }

    public void setGitRepoId(String gitRepoId) {
        this.gitRepoId = gitRepoId;
    }

    public void setReleaseNote(String releaseNote) {
        this.releaseNote = releaseNote;
    }

    public void setHttpUserAgent(String httpUserAgent) {
        this.httpUserAgent = httpUserAgent;
    }

    public void setReservedWordsMappings(List<String> reservedWordsMappings) {
        this.reservedWordsMappings = reservedWordsMappings;
    }

    public void setIgnoreFileOverride(String ignoreFileOverride) {
        this.ignoreFileOverride = ignoreFileOverride;
    }

    public void setRemoveOperationIdPrefix(Boolean removeOperationIdPrefix) {
        this.removeOperationIdPrefix = removeOperationIdPrefix;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    @Override
    public void run() {

        loadArguments();

        // attempt to read from config file
        CodegenConfigurator configurator = CodegenConfigurator.fromFile(configFile);

        // if a config file wasn't specified or we were unable to read it
        if (configurator == null) {
            // createa a fresh configurator
            configurator = new CodegenConfigurator();
        }

        // now override with any specified parameters
        if (verbose != null) {
            configurator.setVerbose(verbose);
        }

        if (skipOverwrite != null) {
            configurator.setSkipOverwrite(skipOverwrite);
        }

        if (isNotEmpty(spec)) {
            configurator.setInputSpec(spec);
        }

        if (isNotEmpty(lang)) {
            configurator.setLang(lang);
        }

        if (isNotEmpty(output)) {
            configurator.setOutputDir(output);
        }

        if (isNotEmpty(auth)) {
            configurator.setAuth(auth);
        }

        if (isNotEmpty(templateDir)) {
            configurator.setTemplateDir(templateDir);
        }

        if (isNotEmpty(apiPackage)) {
            configurator.setApiPackage(apiPackage);
        }

        if (isNotEmpty(modelPackage)) {
            configurator.setModelPackage(modelPackage);
        }

        if (isNotEmpty(modelNamePrefix)) {
            configurator.setModelNamePrefix(modelNamePrefix);
        }

        if (isNotEmpty(modelNameSuffix)) {
            configurator.setModelNameSuffix(modelNameSuffix);
        }

        if (isNotEmpty(invokerPackage)) {
            configurator.setInvokerPackage(invokerPackage);
        }

        if (isNotEmpty(groupId)) {
            configurator.setGroupId(groupId);
        }

        if (isNotEmpty(artifactId)) {
            configurator.setArtifactId(artifactId);
        }

        if (isNotEmpty(artifactVersion)) {
            configurator.setArtifactVersion(artifactVersion);
        }

        if (isNotEmpty(library)) {
            configurator.setLibrary(library);
        }

        if (isNotEmpty(gitUserId)) {
            configurator.setGitUserId(gitUserId);
        }

        if (isNotEmpty(gitRepoId)) {
            configurator.setGitRepoId(gitRepoId);
        }

        if (isNotEmpty(releaseNote)) {
            configurator.setReleaseNote(releaseNote);
        }

        if (isNotEmpty(httpUserAgent)) {
            configurator.setHttpUserAgent(httpUserAgent);
        }

        if (isNotEmpty(ignoreFileOverride)) {
            configurator.setIgnoreFileOverride(ignoreFileOverride);
        }

        if (removeOperationIdPrefix != null) {
            configurator.setRemoveOperationIdPrefix(removeOperationIdPrefix);
        }

        applySystemPropertiesKvpList(systemProperties, configurator);
        applyInstantiationTypesKvpList(instantiationTypes, configurator);
        applyImportMappingsKvpList(importMappings, configurator);
        applyTypeMappingsKvpList(typeMappings, configurator);
        applyAdditionalPropertiesKvpList(additionalProperties, configurator);
        applyLanguageSpecificPrimitivesCsvList(languageSpecificPrimitives, configurator);
        applyReservedWordsMappingsKvpList(reservedWordsMappings, configurator);
        final ClientOptInput clientOptInput = configurator.toClientOptInput();

        new DefaultGenerator().opts(clientOptInput).generate();
    }

    private void loadArguments() {
        if (StringUtils.isBlank(this.url)) {
            return;
        }
        final String content;
        File file = new File(this.url);
        if (file.exists() && file.isFile()) {
            try {
                content = FileUtils.readFileToString(file);
            } catch (IOException e) {
                LOG.error("Unable to read file: " + this.url, e);
                return;
            }
        } else if (isValidURL(this.url)) {
            try {
                content = RemoteUrl.urlToString(this.url, null);
            } catch (Exception e) {
                LOG.error("Unable to read url: " + this.url, e);
                return;
            }
        } else {
            return;
        }

        if (StringUtils.isBlank(content)) {
            return;
        }

        JsonNode node = null;

        if (isValidJson(content)) {
            try {
                node = Json.mapper().readTree(content.getBytes());
            } catch (IOException e) {
                LOG.error("Unable to deserialize json from: " + this.url, e);
                node = null;
            }
        } else if (isValidYaml(content)) {
            try {
                node = Yaml.mapper().readTree(content.getBytes());
            } catch (IOException e) {
                LOG.error("Unable to deserialize yaml from: " + this.url, e);
                node = null;
            }
        }

        if (node == null) {
            return;
        }

        final Map<String, Object> optionValueMap = CLIHelper.createOptionValueMap(node);
        try {
            BeanUtils.populate(this, optionValueMap);
        } catch (Exception e) {
            LOG.error("Error setting values to object.", e);
        }
    }
}
