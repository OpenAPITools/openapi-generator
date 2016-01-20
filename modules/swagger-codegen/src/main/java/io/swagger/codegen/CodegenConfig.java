package io.swagger.codegen;

import io.swagger.models.Model;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.models.auth.SecuritySchemeDefinition;
import io.swagger.models.properties.Property;

import java.util.List;
import java.util.Map;
import java.util.Set;

public interface CodegenConfig {
    CodegenType getTag();

    String getName();

    String getHelp();

    Map<String, Object> additionalProperties();

    String testPackage();

    String apiPackage();

    String apiFileFolder();

    String apiTestFileFolder();

    String fileSuffix();

    String outputFolder();

    String templateDir();

    String embeddedTemplateDir();

    String modelFileFolder();

    String modelTestFileFolder();

    String modelPackage();

    String toApiName(String name);

    String toApiVarName(String name);

    String toModelName(String name);

    String toParamName(String name);

    String escapeText(String text);

    String escapeReservedWord(String name);

    String getTypeDeclaration(Property p);

    String getTypeDeclaration(String name);

    void processOpts();

    List<CliOption> cliOptions();

    String generateExamplePath(String path, Operation operation);

    Set<String> reservedWords();

    List<SupportingFile> supportingFiles();

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

    Set<String> languageSpecificPrimitives();

    void preprocessSwagger(Swagger swagger);

    void processSwagger(Swagger swagger);

    String toApiFilename(String name);

    String toModelFilename(String name);

    String toApiTestFilename(String name);

    String toModelTestFilename(String name);
    
    String toModelImport(String name);

    String toApiImport(String name);

    void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations);

    Map<String, Object> postProcessModels(Map<String, Object> objs);

    Map<String, Object> postProcessOperations(Map<String, Object> objs);

    Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs);

    void postProcessModelProperty(CodegenModel model, CodegenProperty property);

    void postProcessParameter(CodegenParameter parameter);

    String apiFilename(String templateName, String tag);

    String apiTestFilename(String templateName, String tag);

    boolean shouldOverwrite(String filename);

    boolean isSkipOverwrite();

    void setSkipOverwrite(boolean skipOverwrite);

    Map<String, String> supportedLibraries();

    void setLibrary(String library);

    /**
     * Library template (sub-template).
     */
    String getLibrary();
}
