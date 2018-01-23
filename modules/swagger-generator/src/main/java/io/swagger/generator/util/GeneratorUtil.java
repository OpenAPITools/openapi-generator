package io.swagger.generator.util;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.generator.model.GenerationRequest;
import io.swagger.generator.model.Options;
import io.swagger.v3.core.util.Json;
import org.apache.commons.lang3.Validate;

import java.util.ArrayList;
import java.util.List;

import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyAdditionalPropertiesKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyImportMappingsKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyInstantiationTypesKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyLanguageSpecificPrimitivesCsvList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyReservedWordsMappingsKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applySystemPropertiesKvpList;
import static io.swagger.codegen.config.CodegenConfiguratorUtils.applyTypeMappingsKvpList;
import static org.apache.commons.lang3.StringUtils.isNotEmpty;

public class GeneratorUtil {


    public static ClientOptInput getClientOptInput(GenerationRequest generationRequest, String outputDir) {
        final Options options = generationRequest.getOptions();
        String inputSpec = Json.pretty(generationRequest.getSpec());
        Validate.notEmpty(options.getLang(), "language must be specified");
        Validate.notEmpty(inputSpec, "input spec must be specified");

        CodegenConfigurator configurator = new CodegenConfigurator();

        configurator.setOutputDir(outputDir);

        if (isNotEmpty(options.getLang())) {
            configurator.setLang(options.getLang());
        }
        if (isNotEmpty(options.getAuth())) {
            configurator.setAuth(options.getAuth());
        }
        if (isNotEmpty(options.getApiPackage())) {
            configurator.setApiPackage(options.getApiPackage());
        }
        if (isNotEmpty(options.getModelPackage())) {
            configurator.setModelPackage(options.getModelPackage());
        }
        if (isNotEmpty(options.getModelNamePrefix())) {
            configurator.setModelNamePrefix(options.getModelNamePrefix());
        }
        if (isNotEmpty(options.getModelNameSuffix())) {
            configurator.setModelNameSuffix(options.getModelNameSuffix());
        }
        if (isNotEmpty(options.getInvokerPackage())) {
            configurator.setInvokerPackage(options.getInvokerPackage());
        }
        if (isNotEmpty(options.getGroupId())) {
            configurator.setGroupId(options.getGroupId());
        }
        if (isNotEmpty(options.getArtifactId())) {
            configurator.setArtifactId(options.getArtifactId());
        }
        if (isNotEmpty(options.getArtifactVersion())) {
            configurator.setArtifactVersion(options.getArtifactVersion());
        }
        if (isNotEmpty(options.getLibrary())) {
            configurator.setLibrary(options.getLibrary());
        }
        if (isNotEmpty(options.getGitUserId())) {
            configurator.setGitUserId(options.getGitUserId());
        }
        if (isNotEmpty(options.getGitRepoId())) {
            configurator.setGitRepoId(options.getGitRepoId());
        }
        if (isNotEmpty(options.getReleaseNote())) {
            configurator.setReleaseNote(options.getReleaseNote());
        }
        if (isNotEmpty(options.getHttpUserAgent())) {
            configurator.setHttpUserAgent(options.getHttpUserAgent());
        }
        if (options.getRemoveOperationIdPrefix() != null) {
            configurator.setRemoveOperationIdPrefix(options.getRemoveOperationIdPrefix());
        }
        if (options.getSystemProperties() != null) {
            applySystemPropertiesKvpList(options.getSystemProperties(), configurator);
        }
        if (options.getInstantiationTypes() != null) {
            applyInstantiationTypesKvpList(options.getInstantiationTypes(), configurator);
        }
        if (options.getImportMappings() != null) {
            applyImportMappingsKvpList(options.getImportMappings(), configurator);
        }
        if (options.getTypeMappings() != null) {
            applyTypeMappingsKvpList(options.getTypeMappings(), configurator);
        }
        if (options.getAdditionalProperties() != null) {
            applyAdditionalPropertiesKvpList(options.getAdditionalProperties(), configurator);
        }
        if (options.getLanguageSpecificPrimitives() != null) {
            applyLanguageSpecificPrimitivesCsvList(options.getLanguageSpecificPrimitives(), configurator);
        }
        if (options.getReservedWordsMappings() != null) {
            applyReservedWordsMappingsKvpList(options.getReservedWordsMappings(), configurator);
        }

        return configurator.toClientOptInput(inputSpec);
    }

    public static ClientOptInput getClientOptInput(JsonNode node, String outputDir) {

        CodegenConfigurator configurator = new CodegenConfigurator();

        configurator.setOutputDir(outputDir);

        if (node.has("lang")) {
            configurator.setLang(node.findValue("lang").textValue());
        }
        if (node.has("spec")) {
            configurator.setInputSpec(node.findValue("spec").textValue());
        }
        if (node.has("apiPackage")) {
            configurator.setApiPackage(node.findValue("apiPackage").textValue());
        }
        if (node.has("modelPackage")) {
            configurator.setModelPackage(node.findValue("modelPackage").textValue());
        }
        if (node.has("modelNamePrefix")) {
            configurator.setModelNamePrefix(node.findValue("modelNamePrefix").textValue());
        }
        if (node.has("modelNameSuffix")) {
            configurator.setModelNameSuffix(node.findValue("modelNameSuffix").textValue());
        }
        if (node.has("invokerPackage")) {
            configurator.setInvokerPackage(node.findValue("invokerPackage").textValue());
        }
        if (node.has("groupId")) {
            configurator.setGroupId(node.findValue("groupId").textValue());
        }
        if (node.has("artifactId")) {
            configurator.setArtifactId(node.findValue("artifactId").textValue());
        }
        if (node.has("artifactVersion")) {
            configurator.setArtifactVersion(node.findValue("artifactVersion").textValue());
        }
        if (node.has("library")) {
            configurator.setLibrary(node.findValue("library").textValue());
        }
        if (node.has("gitUserId")) {
            configurator.setGitUserId(node.findValue("gitUserId").textValue());
        }
        if (node.has("gitRepoId")) {
            configurator.setGitRepoId(node.findValue("gitRepoId").textValue());
        }
        if (node.has("releaseNote")) {
            configurator.setReleaseNote(node.findValue("releaseNote").textValue());
        }
        if (node.has("httpUserAgent")) {
            configurator.setHttpUserAgent(node.findValue("httpUserAgent").textValue());
        }
        if (node.has("removeOperationIdPrefix")) {
            configurator.setRemoveOperationIdPrefix(node.findValue("removeOperationIdPrefix").booleanValue());
        }

        JsonNode systemPropertiesNode = node.findValue("systemProperties");
        if (systemPropertiesNode != null && systemPropertiesNode.isArray()) {
            List<String> systemProperties = new ArrayList<>();
            for (JsonNode jsonNode : systemPropertiesNode) {
                systemProperties.add(jsonNode.textValue());
            }
            applySystemPropertiesKvpList(systemProperties, configurator);
        }

        JsonNode instantiationTypesNode = node.findValue("instantiationTypes");
        if (instantiationTypesNode != null && instantiationTypesNode.isArray()) {
            List<String> instantiationTypes = new ArrayList<>();
            for (JsonNode jsonNode : instantiationTypesNode) {
                instantiationTypes.add(jsonNode.textValue());
            }
            applyInstantiationTypesKvpList(instantiationTypes, configurator);
        }

        JsonNode importMappingsNode = node.findValue("importMappings");
        if (importMappingsNode != null && importMappingsNode.isArray()) {
            List<String> importMappings = new ArrayList<>();
            for (JsonNode jsonNode : importMappingsNode) {
                importMappings.add(jsonNode.textValue());
            }
            applyImportMappingsKvpList(importMappings, configurator);
        }

        JsonNode typeMappingsNode = node.findValue("typeMappings");
        if (typeMappingsNode != null && typeMappingsNode.isArray()) {
            List<String> typeMappings = new ArrayList<>();
            for (JsonNode jsonNode : typeMappingsNode) {
                typeMappings.add(jsonNode.textValue());
            }
            applyTypeMappingsKvpList(typeMappings, configurator);
        }

        JsonNode additionalPropertiesNode = node.findValue("additionalProperties");
        if (additionalPropertiesNode != null && additionalPropertiesNode.isArray()) {
            List<String> additionalProperties = new ArrayList<>();
            for (JsonNode jsonNode : additionalPropertiesNode) {
                additionalProperties.add(jsonNode.textValue());
            }
            applyAdditionalPropertiesKvpList(additionalProperties, configurator);
        }

        JsonNode languageSpecificPrimitivesNode = node.findValue("languageSpecificPrimitives");
        if (languageSpecificPrimitivesNode != null && languageSpecificPrimitivesNode.isArray()) {
            List<String> languageSpecificPrimitives = new ArrayList<>();
            for (JsonNode jsonNode : languageSpecificPrimitivesNode) {
                languageSpecificPrimitives.add(jsonNode.textValue());
            }
            applyLanguageSpecificPrimitivesCsvList(languageSpecificPrimitives, configurator);
        }

        JsonNode reservedWordMappingsNode = node.findValue("reservedWordMappings");
        if (reservedWordMappingsNode != null && reservedWordMappingsNode.isArray()) {
            List<String> languageSpecificPrimitives = new ArrayList<>();
            for (JsonNode jsonNode : instantiationTypesNode) {
                languageSpecificPrimitives.add(jsonNode.textValue());
            }
            applyReservedWordsMappingsKvpList(languageSpecificPrimitives, configurator);
        }

        return configurator.toClientOptInput();
    }
}
