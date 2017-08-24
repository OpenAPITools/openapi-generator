package io.swagger.codegen;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.samskivert.mustache.Mustache.Compiler;

import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.models.properties.Property;

public interface CodegenConfig {
    CodegenType getTag();

    String getName();

    String getHelp();

    Map<String, Object> additionalProperties();

    Map<String, Object> vendorExtensions();

    String testPackage();

    String apiPackage();

    String apiFileFolder();

    String apiTestFileFolder();

    String apiDocFileFolder();

    String fileSuffix();

    String outputFolder();

    String templateDir();

    String embeddedTemplateDir();

    String modelFileFolder();

    String modelTestFileFolder();

    String modelDocFileFolder();

    String modelPackage();

    String toApiName(String name);

    String toApiVarName(String name);

    String toModelName(String name);

    String toParamName(String name);

    String escapeText(String text);

    String escapeUnsafeCharacters(String input);

    String escapeReservedWord(String name);

    String escapeQuotationMark(String input);

    String getTypeDeclaration(Property p);

    String getTypeDeclaration(String name);

    void processOpts();

    List<CliOption> cliOptions();

    String generateExamplePath(String path, Operation operation);

    Set<String> reservedWords();

    List<SupportingFile> supportingFiles();

    String getInputSpec();

    void setInputSpec(String inputSpec);

    String getOutputDir();

    void setOutputDir(String dir);

    CodegenModel fromModel(String name, Model model);

    CodegenModel fromModel(String name, Model model, Map<String, Model> allDefinitions);

    CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions, Swagger swagger);

    CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, Map<String, Model> definitions);

    List<CodegenSecurity> fromSecurity(Map<String, SecuritySchemeDefinition> schemes);

    Set<String> defaultIncludes();

    Map<String, String> typeMapping();

    Map<String, String> instantiationTypes();

    Map<String, String> importMapping();

    Map<String, String> apiTemplateFiles();

    Map<String, String> modelTemplateFiles();

    Map<String, String> apiTestTemplateFiles();

    Map<String, String> modelTestTemplateFiles();

    Map<String, String> apiDocTemplateFiles();

    Map<String, String> modelDocTemplateFiles();

    Set<String> languageSpecificPrimitives();

    Map<String, String> reservedWordsMappings();

    void preprocessSwagger(Swagger swagger);

    void processSwagger(Swagger swagger);

    Compiler processCompiler(Compiler compiler);

    String sanitizeTag(String tag);

    String toApiFilename(String name);

    String toModelFilename(String name);

    String toApiTestFilename(String name);

    String toModelTestFilename(String name);

    String toApiDocFilename(String name);

    String toModelDocFilename(String name);

    String toModelImport(String name);

    String toApiImport(String name);

    void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations);

    Map<String, Object> postProcessAllModels(Map<String, Object> objs);

    Map<String, Object> postProcessModels(Map<String, Object> objs);

    Map<String, Object> postProcessOperations(Map<String, Object> objs);

    Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels);

    Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs);

    void postProcessModelProperty(CodegenModel model, CodegenProperty property);

    void postProcessParameter(CodegenParameter parameter);

    String apiFilename(String templateName, String tag);

    String apiTestFilename(String templateName, String tag);

    String apiDocFilename(String templateName, String tag);

    boolean shouldOverwrite(String filename);

    boolean isSkipOverwrite();

    void setSkipOverwrite(boolean skipOverwrite);

    boolean isRemoveOperationIdPrefix();

    void setRemoveOperationIdPrefix(boolean removeOperationIdPrefix);

    Map<String, String> supportedLibraries();

    void setLibrary(String library);

    /**
     * Library template (sub-template).
     *
     * @return libray template
     */
    String getLibrary();

    void setGitUserId(String gitUserId);

    String getGitUserId();

    void setGitRepoId(String gitRepoId);

    String getGitRepoId();

    void setReleaseNote(String releaseNote);

    String getReleaseNote();

    void setHttpUserAgent(String httpUserAgent);

    String getHttpUserAgent();

    String getCommonTemplateDir();

    void setIgnoreFilePathOverride(String ignoreFileOverride);

    String getIgnoreFilePathOverride();

    String toBooleanGetter(String name);

    String toSetter(String name);

    String toGetter(String name);

}
