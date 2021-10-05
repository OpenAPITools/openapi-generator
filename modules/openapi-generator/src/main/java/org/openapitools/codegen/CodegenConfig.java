/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import com.samskivert.mustache.Mustache.Compiler;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.oas.models.servers.ServerVariable;
import org.openapitools.codegen.api.TemplatingEngineAdapter;
import org.openapitools.codegen.meta.FeatureSet;
import org.openapitools.codegen.meta.GeneratorMetadata;

import java.io.File;
import java.util.List;
import java.util.Map;
import java.util.Set;

public interface CodegenConfig {
    String getFilesMetadataFilename();

    String getVersionMetadataFilename();

    GeneratorMetadata getGeneratorMetadata();

    CodegenType getTag();

    String getName();

    String getHelp();

    Map<String, Object> additionalProperties();

    Map<String, String> serverVariableOverrides();

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

    String escapeTextWhileAllowingNewLines(String text);

    String encodePath(String text);

    String escapeUnsafeCharacters(String input);

    String escapeReservedWord(String name);

    String escapeQuotationMark(String input);

    String getTypeDeclaration(Schema schema);

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

    CodegenModel fromModel(String name, Schema schema);

    CodegenOperation fromOperation(String resourcePath, String httpMethod, Operation operation, List<Server> servers);

    List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> schemas);

    List<CodegenServer> fromServers(List<Server> servers);

    List<CodegenServerVariable> fromServerVariables(Map<String, ServerVariable> variables);

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

    void preprocessOpenAPI(OpenAPI openAPI);

    void processOpenAPI(OpenAPI openAPI);

    Compiler processCompiler(Compiler compiler);

    TemplatingEngineAdapter processTemplatingEngine(TemplatingEngineAdapter templatingEngine);

    String sanitizeTag(String tag);

    String toApiFilename(String name);

    String toModelFilename(String name);

    String toApiTestFilename(String name);

    String toModelTestFilename(String name);

    String toApiDocFilename(String name);

    String toModelDocFilename(String name);

    String toModelImport(String name);

    Map<String,String> toModelImportMap(String name);

    String toApiImport(String name);

    void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations);

    Map<String, Object> updateAllModels(Map<String, Object> objs);

    void postProcess();

    Map<String, Object> postProcessAllModels(Map<String, Object> objs);

    Map<String, Object> postProcessModels(Map<String, Object> objs);

    Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels);

    Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs);

    void postProcessModelProperty(CodegenModel model, CodegenProperty property);

    void postProcessParameter(CodegenParameter parameter);

    String modelFilename(String templateName, String modelName);

    String apiFilename(String templateName, String tag);

    String apiTestFilename(String templateName, String tag);

    String apiDocFilename(String templateName, String tag);

    boolean shouldOverwrite(String filename);

    boolean isSkipOverwrite();

    void setSkipOverwrite(boolean skipOverwrite);

    boolean isRemoveOperationIdPrefix();

    void setRemoveOperationIdPrefix(boolean removeOperationIdPrefix);

    boolean isSkipOperationExample();

    void setSkipOperationExample(boolean skipOperationExample);

    public boolean isHideGenerationTimestamp();

    public void setHideGenerationTimestamp(boolean hideGenerationTimestamp);

    Map<String, String> supportedLibraries();

    void setLibrary(String library);

    /**
     * Library template (sub-template).
     *
     * @return library template
     */
    String getLibrary();

    void setGitHost(String gitHost);

    String getGitHost();

    void setGitUserId(String gitUserId);

    String getGitUserId();

    void setGitRepoId(String gitRepoId);

    String getGitRepoId();

    void setReleaseNote(String releaseNote);

    String getReleaseNote();

    void setHttpUserAgent(String httpUserAgent);

    String getHttpUserAgent();

    void setDocExtension(String docExtension);

    String getDocExtension();

    void setIgnoreFilePathOverride(String ignoreFileOverride);

    String getIgnoreFilePathOverride();

    String toBooleanGetter(String name);

    String toSetter(String name);

    String toGetter(String name);

    String sanitizeName(String name);

    void postProcessFile(File file, String fileType);

    boolean isEnablePostProcessFile();

    void setEnablePostProcessFile(boolean isEnablePostProcessFile);

    /**
     * Set the OpenAPI instance. This method needs to be called right after the instantiation of the Codegen class.
     * @param openAPI specification being generated
     */
    void setOpenAPI(OpenAPI openAPI);

    void setTemplatingEngine(TemplatingEngineAdapter s);

    TemplatingEngineAdapter getTemplatingEngine();

    public boolean isEnableMinimalUpdate();

    public void setEnableMinimalUpdate(boolean isEnableMinimalUpdate);

    boolean isStrictSpecBehavior();

    void setStrictSpecBehavior(boolean strictSpecBehavior);

    FeatureSet getFeatureSet();

    boolean isRemoveEnumValuePrefix();

    void setRemoveEnumValuePrefix(boolean removeEnumValuePrefix);

    Schema unaliasSchema(Schema schema, Map<String, String> usedImportMappings);
}
