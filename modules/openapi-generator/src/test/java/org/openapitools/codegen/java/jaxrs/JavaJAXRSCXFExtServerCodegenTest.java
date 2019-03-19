package org.openapitools.codegen.java.jaxrs;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

import java.io.File;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.ClientOpts;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.MockDefaultGenerator;
import org.openapitools.codegen.MockDefaultGenerator.WrittenTemplateBasedFile;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.AbstractJavaCodegen;
import org.openapitools.codegen.languages.AbstractJavaJAXRSServerCodegen;
import org.openapitools.codegen.languages.JavaCXFExtServerCodegen;
import org.openapitools.codegen.languages.features.BeanValidationExtendedFeatures;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.languages.features.CXFExtServerFeatures;
import org.openapitools.codegen.languages.features.CXFServerFeatures;
import org.openapitools.codegen.languages.features.GzipFeatures;
import org.openapitools.codegen.languages.features.GzipTestFeatures;
import org.openapitools.codegen.languages.features.JbossFeature;
import org.openapitools.codegen.languages.features.LoggingFeatures;
import org.openapitools.codegen.languages.features.LoggingTestFeatures;
import org.openapitools.codegen.languages.features.SpringFeatures;
import org.openapitools.codegen.languages.features.SwaggerFeatures;
import org.openapitools.codegen.languages.features.SwaggerUIFeatures;
import org.openapitools.codegen.languages.features.UseGenericResponseFeatures;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.servers.Server;
import io.swagger.v3.parser.core.models.ParseOptions;

public class JavaJAXRSCXFExtServerCodegenTest {
    @SuppressWarnings("unused")
    private static class JavaCXFExtServerCodegenTester extends JavaCXFExtServerCodegen {
        /* Options processed by DefaultCodegen */

        // CodegenConstants.ALLOW_UNICODE_IDENTIFIERS
        public Boolean getAllowUnicodeIdentifiers() {
            return allowUnicodeIdentifiers;
        }

        // CodegenConstants.API_PACKAGE
        public String getApiPackage() {
            return apiPackage;
        }

        // CodegenConstants.DOCEXTENSION
//        String getDocExtension();

        // CodegenConstants.ARTIFACT_DESCRIPTION
        public String getArtifactDescription() {
            return artifactDescription;
        }

        // CodegenConstants.ARTIFACT_ID
        public String getArtifactId() {
            return artifactId;
        }

        // CodegenConstants.ARTIFACT_URL
        public String getArtifactUrl() {
            return artifactUrl;
        }

        // CodegenConstants.ARTIFACT_VERSION
        public String getArtifactVersion() {
            return artifactVersion;
        }

        // AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX
        public String getBooleanGetterPrefix() {
            return booleanGetterPrefix;
        }

        // AbstractJavaCodegen.DATE_LIBRARY
        public String getDateLibrary() {
            return dateLibrary;
        }

        // CodegenConstants.DEVELOPER_EMAIL
        public String getDeveloperEmail() {
            return developerEmail;
        }

        // CodegenConstants.DEVELOPER_NAME
        public String getDeveloperName() {
            return developerName;
        }

        // CodegenConstants.DEVELOPER_ORGANIZATION
        public String getDeveloperOrganization() {
            return developerOrganization;
        }

        // CodegenConstants.DEVELOPER_ORGANIZATION_URL
        public String getDeveloperOrganizationUrl() {
            return developerOrganizationUrl;
        }

        /* Options processed by AbstractJavaCodegen */

        // CodegenConstants.ENSURE_UNIQUE_PARAMS
        public Boolean getEnsureUniqueParams() {
            return ensureUniqueParams;
        }

        // CodegenConstants.GROUP_ID
        public String getGroupId() {
            return groupId;
        }

        // CodegenConstants.HIDE_GENERATION_TIMESTAMP
        public Boolean getHideGenerationTimestamp() {
            return hideGenerationTimestamp;
        }

        // CodegenConstants.IMPL_FOLDER
        public String getImplFolder() {
            return implFolder;
        }

        // CodegenConstants.INVOKER_PACKAGE
        public String getInvokerPackage() {
            return invokerPackage;
        }

        // CodegenConstants.LICENSE_NAME
        public String getLicenseName() {
            return licenseName;
        }

        // CodegenConstants.LICENSE_URL
        public String getLicenseUrl() {
            return licenseUrl;
        }

        // CodegenConstants.LOCAL_VARIABLE_PREFIX
        public String getLocalVariablePrefix() {
            return localVariablePrefix;
        }

        // CodegenConstants.MODEL_NAME_PREFIX
        public String getModelNamePrefix() {
            return modelNamePrefix;
        }

        // CodegenConstants.MODEL_NAME_SUFFIX
        public String getModelNameSuffix() {
            return modelNameSuffix;
        }

        // CodegenConstants.MODEL_PACKAGE
        public String getModelPackage() {
            return modelPackage;
        }

        // CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS
        public Boolean getPrependFormOrBodyParameters() {
            return prependFormOrBodyParameters;
        }

        // CodegenConstants.REMOVE_OPERATION_ID_PREFIX
        boolean getRemoveOperationIdPrefix() {
            return removeOperationIdPrefix;
        }

        // CodegenConstants.SCM_CONNECTION
        public String getScmConnection() {
            return scmConnection;
        }

        // CodegenConstants.SCM_DEVELOPER_CONNECTION
        public String getScmDeveloperConnection() {
            return scmDeveloperConnection;
        }

        // CodegenConstants.SCM_URL
        public String getScmUrl() {
            return scmUrl;
        }

        // CodegenConstants.SERIALIZABLE_MODEL
        public Boolean getSerializableModel() {
            return serializableModel;
        }

        // CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG
        public Boolean getSortParamsByRequiredFlag() {
            return sortParamsByRequiredFlag;
        }

        // CodegenConstants.SOURCE_FOLDER
        public String getSourceFolder() {
            return sourceFolder;
        }

        // AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX_DEFAULT (UNUSED)

        // CodegenConstants.TEMPLATE_DIR
        public String getTemplateDir() {
            return templateDir;
        }

        // AbstractJavaCodegen.DEFAULT_LIBRARY (UNUSED)

        // CXFServerFeatures.TEST_DATA_CONTROL_FILE
        public File getTestDataControlFile() {
            return testDataControlFile;
        }

        // CXFServerFeatures.ADD_CONSUMES_PRODUCES_JSON
        public boolean isAddConsumesProducesJson() {
            return addConsumesProducesJson;
        }

        // AbstractJavaCodegen.DISABLE_HTML_ESCAPING
        public boolean isDisableHtmlEscaping() {
            return disableHtmlEscaping;
        }

        // AbstractJavaCodegen.FULL_JAVA_UTIL
        public boolean isFullJavaUtil() {
            return fullJavaUtil;
        }

        // JbossFeature.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR
        public boolean isGenerateJbossDeploymentDescriptor() {
            return generateJbossDeploymentDescriptor;
        }

        // CXFServerFeatures.GENERATE_NON_SPRING_APPLICATION
        public boolean isGenerateNonSpringApplication() {
            return generateNonSpringApplication;
        }

        /* Options processed by AbstractJavaJAXRSServerCodegen */

        // SpringFeatures.GENERATE_SPRING_APPLICATION
        public boolean isGenerateSpringApplication() {
            return generateSpringApplication;
        }

        // AbstractJavaJAXRSServerCodegen.SERVER_PORT (no corresponding field)

        /* Options processed by JavaCXFServerCodegen */

        // SpringFeatures.GENERATE_SPRING_BOOT_APPLICATION
        public boolean isGenerateSpringBootApplication() {
            return generateSpringBootApplication;
        }

        // AbstractJavaCodegen.JAVA8_MODE
        public boolean isJava8Mode() {
            return java8Mode;
        }

        // CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING
        public boolean isSerializeBigDecimalAsString() {
            return serializeBigDecimalAsString;
        }

        // AbstractJavaCodegen.SUPPORT_ASYNC
        public boolean isSupportAsync() {
            return supportAsync;
        }

        // AbstractJavaCodegen.SUPPORT_JAVA6
        public boolean isSupportJava6() {
            return supportJava6;
        }

        // CXFServerFeatures.USE_ANNOTATED_BASE_PATH
        public boolean isUseAnnotatedBasePath() {
            return useAnnotatedBasePath;
        }

        // BeanValidationFeatures.USE_BEANVALIDATION
        public boolean isUseBeanValidation() {
            return useBeanValidation;
        }

        // BeanValidationExtendedFeatures.USE_BEANVALIDATION_FEATURE
        public boolean isUseBeanValidationFeature() {
            return useBeanValidationFeature;
        }

        // UseGenericResponseFeatures.USE_GENERIC_RESPONSE
        public boolean isUseGenericResponse() {
            return useGenericResponse;
        }

        // GzipFeatures.USE_GZIP_FEATURE
        public boolean isUseGzipFeature() {
            return useGzipFeature;
        }

        // GzipTestFeatures.USE_GZIP_FEATURE_FOR_TESTS
        public boolean isUseGzipFeatureForTests() {
            return useGzipFeatureForTests;
        }

        // LoggingFeatures.USE_LOGGING_FEATURE
        public boolean isUseLoggingFeature() {
            return useLoggingFeature;
        }

        // LoggingTestFeatures.USE_LOGGING_FEATURE_FOR_TESTS
        public boolean isUseLoggingFeatureForTests() {
            return useLoggingFeatureForTests;
        }

        // CXFServerFeatures.USE_MULTIPART_FEATURE
        public boolean isUseMultipartFeature() {
            return useMultipartFeature;
        }

        // AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE
        public boolean isUseNullForUnknownEnumValue() {
            return useNullForUnknownEnumValue;
        }

        // SpringFeatures.USE_SPRING_ANNOTATION_CONFIG
        public boolean isUseSpringAnnotationConfig() {
            return useSpringAnnotationConfig;
        }

        // SwaggerFeatures.USE_SWAGGER_FEATURE
        public boolean isUseSwaggerFeature() {
            return useSwaggerFeature;
        }

        // SwaggerUIFeatures.USE_SWAGGER_UI
        public boolean isUseSwaggerUI() {
            return useSwaggerUI;
        }

        // CXFServerFeatures.USE_WADL_FEATURE
        public boolean isUseWadlFeature() {
            return useWadlFeature;
        }

        // AbstractJavaCodegen.WITH_XML
        public boolean isWithXml() {
            return withXml;
        }

        /* Options processed by JavaCXFExtServerCodegen */

        // CXFExtServerFeatures.GENERATE_OPERATION_BODY
        public boolean isGenerateOperationBody() {
            return generateOperationBody;
        }

        // CXFExtServerFeatures.TEST_DATA_FILE
        public File getTestDataFile() {
            return testDataFile;
        }

        // CXFExtServerFeatures.LOAD_TEST_DATA_FROM_FILE
        public boolean isLoadTestDataFromFile() {
            return loadTestDataFromFile;
        }

        // CXFExtServerFeatures.SUPPORT_MULTIPLE_SPRING_SERVICES
        public boolean isSupportMultipleSpringServices() {
            return supportMultipleSpringServices;
        }
    }

    private JavaCXFExtServerCodegenTester codegen;

    @BeforeMethod
    public void beforeMethod() {
        codegen = new JavaCXFExtServerCodegenTester();
    }

    private void checkFile(MockDefaultGenerator generator, String path, boolean fileShouldExist, String... regexes) {
        String file = generator.getFiles().get(path);
        if (fileShouldExist)
            assertNotNull(file);
        else
            assertNull(file);
        for (String regex : regexes)
            assertTrue(Pattern.compile(regex).matcher(file).find());
    }

    @SuppressWarnings("unchecked")
    private List<CodegenOperation> getOperationsList(Map<String, Object> templateData) {
        assertTrue(templateData.get("operations") instanceof Map);
        Map<String, Object> operations = (Map<String, Object>) templateData.get("operations");
        assertTrue(operations.get("operation") instanceof List);
        return (List<CodegenOperation>) operations.get("operation");
    }

    @Test
    public void testAdditionalPropertiesPutForConfigValues() throws Exception {
        Map<String, Object> additionalProperties = codegen.additionalProperties();

        // Options processed by DefaultCodegen
        additionalProperties.put(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS, "true");
        additionalProperties.put(CodegenConstants.API_PACKAGE, "xyz.yyyyy.aaaaa.api");
        additionalProperties.put(CodegenConstants.DOCEXTENSION, "doc");
        additionalProperties.put(CodegenConstants.ENSURE_UNIQUE_PARAMS, "true");
        additionalProperties.put(CodegenConstants.HIDE_GENERATION_TIMESTAMP, "true");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, "xyz.yyyyy.iiii.invoker");
        additionalProperties.put(CodegenConstants.MODEL_NAME_PREFIX, "MyPrefix");
        additionalProperties.put(CodegenConstants.MODEL_NAME_SUFFIX, "MySuffix");
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, "xyz.yyyyy.mmmmm.model");
        additionalProperties.put(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS, "true");
        additionalProperties.put(CodegenConstants.REMOVE_OPERATION_ID_PREFIX, "true");
        additionalProperties.put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "true");
        additionalProperties.put(CodegenConstants.TEMPLATE_DIR, "MyTemplates");
        // Options processed by AbstractJavaCodegen
        additionalProperties.put(CodegenConstants.ARTIFACT_DESCRIPTION, "My description");
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, "my-artifact");
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, "9.9.9");
        additionalProperties.put(CodegenConstants.ARTIFACT_URL, "http://organisation.org/group/artifact");
        additionalProperties.put(CodegenConstants.DEVELOPER_EMAIL, "dchappie@organisation.org");
        additionalProperties.put(CodegenConstants.DEVELOPER_NAME, "Developer Chappie");
        additionalProperties.put(CodegenConstants.DEVELOPER_ORGANIZATION, "My Organisation");
        additionalProperties.put(CodegenConstants.DEVELOPER_ORGANIZATION_URL, "http://www.organisation.org/");
        additionalProperties.put(CodegenConstants.GROUP_ID, "org.organisation.group.id");
        additionalProperties.put(CodegenConstants.LICENSE_NAME, "Apache 2.0");
        additionalProperties.put(CodegenConstants.LICENSE_URL, "https://www.apache.org/licenses/LICENSE-2.0");
        additionalProperties.put(CodegenConstants.SCM_CONNECTION, "http://svn.organisation.org/group/");
        additionalProperties.put(CodegenConstants.SCM_DEVELOPER_CONNECTION, "http://svn.organisation.org/dev/group/");
        additionalProperties.put(CodegenConstants.SCM_URL, "http://svn.organisation.org/group/");
        additionalProperties.put(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING, "true");
        additionalProperties.put(CodegenConstants.SERIALIZABLE_MODEL, "true");
        additionalProperties.put(CodegenConstants.SOURCE_FOLDER, "src/main/java");
        additionalProperties.put(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX, "isIt");
        additionalProperties.put(AbstractJavaCodegen.DATE_LIBRARY, "MyDateLibrary");
        additionalProperties.put(AbstractJavaCodegen.DISABLE_HTML_ESCAPING, "true");
        additionalProperties.put(AbstractJavaCodegen.FULL_JAVA_UTIL, "true");
        additionalProperties.put(AbstractJavaCodegen.JAVA8_MODE, "true");
        additionalProperties.put(AbstractJavaCodegen.SUPPORT_ASYNC, "true");
        additionalProperties.put(AbstractJavaCodegen.SUPPORT_JAVA6, "false");
        additionalProperties.put(AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE, "true");
        additionalProperties.put(AbstractJavaCodegen.WITH_XML, "true");
        // Options processed by AbstractJavaJAXRSServerCodegen
        additionalProperties.put(CodegenConstants.IMPL_FOLDER, "myimpl");
        additionalProperties.put(BeanValidationFeatures.USE_BEANVALIDATION, "true");
        additionalProperties.put(AbstractJavaJAXRSServerCodegen.SERVER_PORT, "8088");
        // Options processed by JavaCXFServerCodegen
        additionalProperties.put(BeanValidationExtendedFeatures.USE_BEANVALIDATION_FEATURE, Boolean.TRUE);
        additionalProperties.put(GzipFeatures.USE_GZIP_FEATURE, Boolean.TRUE);
        additionalProperties.put(GzipTestFeatures.USE_GZIP_FEATURE_FOR_TESTS, "true");
        additionalProperties.put(JbossFeature.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR, Boolean.TRUE);
        additionalProperties.put(LoggingFeatures.USE_LOGGING_FEATURE, Boolean.TRUE);
        additionalProperties.put(LoggingTestFeatures.USE_LOGGING_FEATURE_FOR_TESTS, "true");
        additionalProperties.put(SpringFeatures.GENERATE_SPRING_APPLICATION, Boolean.TRUE);
        additionalProperties.put(SpringFeatures.GENERATE_SPRING_BOOT_APPLICATION, Boolean.TRUE);
        additionalProperties.put(SpringFeatures.USE_SPRING_ANNOTATION_CONFIG, Boolean.TRUE);
        additionalProperties.put(SwaggerFeatures.USE_SWAGGER_FEATURE, Boolean.TRUE);
        additionalProperties.put(SwaggerUIFeatures.USE_SWAGGER_UI, Boolean.TRUE);
        additionalProperties.put(UseGenericResponseFeatures.USE_GENERIC_RESPONSE, "true");
        additionalProperties.put(CXFServerFeatures.ADD_CONSUMES_PRODUCES_JSON, Boolean.TRUE);
        additionalProperties.put(CXFServerFeatures.GENERATE_NON_SPRING_APPLICATION, Boolean.FALSE);
        additionalProperties.put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, Boolean.TRUE);
        additionalProperties.put(CXFServerFeatures.USE_ANNOTATED_BASE_PATH, Boolean.TRUE);
        additionalProperties.put(CXFServerFeatures.USE_MULTIPART_FEATURE, Boolean.TRUE);
        additionalProperties.put(CXFServerFeatures.USE_WADL_FEATURE, Boolean.TRUE);
        // Options processed by JavaCXFExtServerCodegen
        additionalProperties.put(CXFExtServerFeatures.GENERATE_OPERATION_BODY, Boolean.TRUE);
        additionalProperties.put(CXFExtServerFeatures.SUPPORT_MULTIPLE_SPRING_SERVICES, Boolean.TRUE);
        additionalProperties.put(CXFExtServerFeatures.TEST_DATA_FILE, "my/test-data.json");
        additionalProperties.put(CXFExtServerFeatures.TEST_DATA_CONTROL_FILE, "my/test-data-control.json");

        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        // Options processed by DefaultCodegen
        assertEquals(codegen.getAllowUnicodeIdentifiers(), Boolean.TRUE);
        assertEquals(codegen.apiPackage(), "xyz.yyyyy.aaaaa.api");
        assertEquals(codegen.getDocExtension(), "doc");
        assertEquals(codegen.getEnsureUniqueParams(), Boolean.TRUE);
        assertEquals(codegen.isHideGenerationTimestamp(), true);
        assertEquals(codegen.getInvokerPackage(), "xyz.yyyyy.iiii.invoker");
        assertEquals(codegen.getModelNamePrefix(), "MyPrefix");
        assertEquals(codegen.getModelNameSuffix(), "MySuffix");
        assertEquals(codegen.getModelPackage(), "xyz.yyyyy.mmmmm.model");
        assertEquals(codegen.getPrependFormOrBodyParameters(), Boolean.TRUE);
        assertEquals(codegen.getRemoveOperationIdPrefix(), true);
        assertEquals(codegen.getSortParamsByRequiredFlag(), Boolean.TRUE);
        assertEquals(codegen.getTemplateDir(), "MyTemplates");
        assertEquals(codegen.getArtifactDescription(), "My description");
        // Options processed by AbstractJavaCodegen
        assertEquals(codegen.getArtifactId(), "my-artifact");
        assertEquals(codegen.getArtifactVersion(), "9.9.9");
        assertEquals(codegen.getArtifactUrl(), "http://organisation.org/group/artifact");
        assertEquals(codegen.getDeveloperEmail(), "dchappie@organisation.org");
        assertEquals(codegen.getDeveloperName(), "Developer Chappie");
        assertEquals(codegen.getDeveloperOrganization(), "My Organisation");
        assertEquals(codegen.getDeveloperOrganizationUrl(), "http://www.organisation.org/");
        assertEquals(codegen.getGroupId(), "org.organisation.group.id");
        assertEquals(codegen.getLicenseName(), "Apache 2.0");
        assertEquals(codegen.getLicenseUrl(), "https://www.apache.org/licenses/LICENSE-2.0");
        assertEquals(codegen.getScmConnection(), "http://svn.organisation.org/group/");
        assertEquals(codegen.getScmDeveloperConnection(), "http://svn.organisation.org/dev/group/");
        assertEquals(codegen.getScmUrl(), "http://svn.organisation.org/group/");
        assertEquals(codegen.isSerializeBigDecimalAsString(), true);
        assertEquals(codegen.getSerializableModel(), Boolean.TRUE);
        assertEquals(codegen.getSourceFolder(), "src/main/java");
        assertEquals(codegen.getBooleanGetterPrefix(), "isIt");
        assertEquals(codegen.getDateLibrary(), "MyDateLibrary");
        assertEquals(codegen.isDisableHtmlEscaping(), true);
        assertEquals(codegen.isFullJavaUtil(), true);
        assertEquals(codegen.isJava8Mode(), true);
        assertEquals(codegen.isSupportAsync(), true);
        assertEquals(codegen.isUseNullForUnknownEnumValue(), true);
        assertEquals(codegen.isWithXml(), true);
        // Options processed by AbstractJavaJAXRSServerCodegen
        assertEquals(codegen.getImplFolder(), "myimpl");
        assertEquals(codegen.isUseBeanValidation(), true);
//        assertEquals(codegen.getServerPort(), 8088);
        // Options processed by JavaCXFServerCodegen
        File curdir = new File(System.getProperty("user.dir"));
        assertEquals(codegen.isUseBeanValidationFeature(), true);
        assertEquals(codegen.isUseGzipFeature(), true);
        assertEquals(codegen.isUseGzipFeatureForTests(), true);
        assertEquals(codegen.isGenerateJbossDeploymentDescriptor(), true);
        assertEquals(codegen.isUseLoggingFeature(), true);
        assertEquals(codegen.isUseLoggingFeatureForTests(), true);
        assertEquals(codegen.isGenerateSpringApplication(), true);
        assertEquals(codegen.isGenerateSpringBootApplication(), true);
        assertEquals(codegen.isUseSpringAnnotationConfig(), true);
        assertEquals(codegen.isUseSwaggerFeature(), true);
        assertEquals(codegen.isUseSwaggerUI(), true);
        assertEquals(codegen.isUseGenericResponse(), true);
        assertEquals(codegen.isAddConsumesProducesJson(), true);
        assertEquals(codegen.isGenerateNonSpringApplication(), false);
        assertEquals(codegen.isUseAnnotatedBasePath(), true);
        assertEquals(codegen.isUseMultipartFeature(), true);
        assertEquals(codegen.isUseWadlFeature(), true);
        // Options processed by JavaCXFExtServerCodegen
        assertEquals(codegen.isGenerateOperationBody(), true);
        assertEquals(codegen.isLoadTestDataFromFile(), true);
        assertEquals(codegen.isSupportMultipleSpringServices(), true);
        assertEquals(codegen.getTestDataFile(), new File(curdir, "my/test-data.json"));
        assertEquals(codegen.getTestDataControlFile(), new File(curdir, "my/test-data-control.json"));
    }

    @Test
    public void testAddOperationToGroup() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();

        OpenAPI openAPI = new OpenAPIParser().readLocation("src/test/resources/3_0/tags.yaml", null, new ParseOptions())
                .getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOpts opts = new ClientOpts();
        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);
        input.setOpts(opts);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        WrittenTemplateBasedFile tag1File = TestUtils.getTemplateBasedFile(generator, output,
                "src/gen/java/org/openapitools/api/Tag1Api.java");
        assertEquals(tag1File.getTemplateData().get("baseName"), "Tag1");
        assertEquals(tag1File.getTemplateData().get("commonPath"), "Tag1");
        List<CodegenOperation> tag1List = getOperationsList(tag1File.getTemplateData());
        assertEquals(tag1List.size(), 1);
        assertEquals(tag1List.get(0).path, "/group1/op1");
        assertEquals(tag1List.get(0).baseName, "Tag1");
        assertEquals(tag1List.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile tag2File = TestUtils.getTemplateBasedFile(generator, output,
                "src/gen/java/org/openapitools/api/Tag2Api.java");
        assertEquals(tag2File.getTemplateData().get("baseName"), "Tag2");
        assertEquals(tag2File.getTemplateData().get("commonPath"), "Tag2");
        List<CodegenOperation> tag2List = getOperationsList(tag2File.getTemplateData());
        assertEquals(tag2List.size(), 2);
        assertEquals(tag2List.get(0).path, "/group1/op2");
        assertEquals(tag2List.get(0).baseName, "Tag2");
        assertEquals(tag2List.get(0).subresourceOperation, true);
        assertEquals(tag2List.get(1).path, "/group2/op3");
        assertEquals(tag2List.get(1).baseName, "Tag2");
        assertEquals(tag2List.get(1).subresourceOperation, true);

        WrittenTemplateBasedFile defaultFile = TestUtils.getTemplateBasedFile(generator, output,
                "src/gen/java/org/openapitools/api/DefaultApi.java");
        assertEquals(defaultFile.getTemplateData().get("baseName"), "Default");
        assertEquals(defaultFile.getTemplateData().get("commonPath"), "Default");
        List<CodegenOperation> defaultList = getOperationsList(defaultFile.getTemplateData());
        assertEquals(defaultList.size(), 1);
        assertEquals(defaultList.get(0).path, "/group3/op4");
        assertEquals(defaultList.get(0).baseName, "Default");
        assertEquals(defaultList.get(0).subresourceOperation, true);

        WrittenTemplateBasedFile group4File = TestUtils.getTemplateBasedFile(generator, output,
                "src/gen/java/org/openapitools/api/Group4Api.java");
        assertEquals(group4File.getTemplateData().get("baseName"), "Group4");
        assertEquals(group4File.getTemplateData().get("commonPath"), "Group4");
        List<CodegenOperation> group4List = getOperationsList(group4File.getTemplateData());
        assertEquals(group4List.size(), 2);
        assertEquals(group4List.get(0).path, "/group4/op5");
        assertEquals(group4List.get(0).baseName, "Group4");
        assertEquals(group4List.get(0).subresourceOperation, true);
        assertEquals(group4List.get(1).path, "/group4/op6");
        assertEquals(group4List.get(1).baseName, "Group4");
        assertEquals(group4List.get(1).subresourceOperation, true);

        WrittenTemplateBasedFile group5File = TestUtils.getTemplateBasedFile(generator, output,
                "src/gen/java/org/openapitools/api/Group5Api.java");
        assertEquals(group5File.getTemplateData().get("baseName"), "Group5");
        assertEquals(group5File.getTemplateData().get("commonPath"), "Group5");
        List<CodegenOperation> group5List = getOperationsList(group5File.getTemplateData());
        assertEquals(group5List.size(), 2);
        assertEquals(group5List.get(0).path, "/group5/op7");
        assertEquals(group5List.get(0).baseName, "Group5");
        assertEquals(group5List.get(0).subresourceOperation, true);
        assertEquals(group5List.get(1).path, "/group6/op8");
        assertEquals(group5List.get(1).baseName, "Group5");
        assertEquals(group5List.get(1).subresourceOperation, true);
    }

    @Test()
    public void testGenerateOperationBodyWithCodedTestData() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOpts opts = new ClientOpts();
        opts.getProperties().put(CXFExtServerFeatures.GENERATE_OPERATION_BODY, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);
        input.setOpts(opts);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        String reGetPetById = "(?s)(?m)public Pet getPetById\\(Long petId\\) \\{" // split
                + ".*" // split
                + "Pet response = new Pet\\(\\);" // split
                + ".*" // split
                + "return response;\\s+" // split
                + "\\}"; // split
        checkFile(generator, outputPath + "/src/main/java/org/openapitools/api/impl/PetApiServiceImpl.java", true,
                reGetPetById);

        String reFindPetsByStatusTest = "(?s)(?m)public void findPetsByStatusTest\\(\\) throws Exception \\{\\s+"
                + ".*" // split
                + "List<String> status = new ArrayList<>\\(\\);" // split
                + ".*" // split
                + "List<Pet> response = api\\.findPetsByStatus\\(status\\);" // split
                + ".*" // split
                + "validate\\(response\\);\\s+" // split
                + "\\}";
        checkFile(generator, outputPath + "/src/test/java/org/openapitools/api/PetApiTest.java", true,
                reFindPetsByStatusTest);

        checkFile(generator, outputPath + "/src/main/resources/test-data.json", false);

        checkFile(generator, outputPath + "/test-data-control.json", false);
    }

    @Test()
    public void testGenerateOperationBodyWithJsonTestData() throws Exception {
        File output = Files.createTempDirectory("test").toFile().getCanonicalFile();
        output.deleteOnExit();
        String outputPath = output.getAbsolutePath().replace('\\', '/');

        OpenAPI openAPI = new OpenAPIParser()
                .readLocation("src/test/resources/3_0/petstore.yaml", null, new ParseOptions()).getOpenAPI();
        codegen.setOutputDir(output.getAbsolutePath());

        ClientOpts opts = new ClientOpts();
        opts.getProperties().put(CXFExtServerFeatures.GENERATE_OPERATION_BODY, "true");
        opts.getProperties().put(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE, "true");

        ClientOptInput input = new ClientOptInput();
        input.setOpenAPI(openAPI);
        input.setConfig(codegen);
        input.setOpts(opts);

        MockDefaultGenerator generator = new MockDefaultGenerator();
        generator.opts(input).generate();

        String reInitCache = "(?s)(?m)\\{\\s+" + "try \\{\\s+"
                + "File cacheFile = new File\\(System\\.getProperty\\(\"jaxrs\\.test\\.server\\.json\",\\s+\"(.+)\"\\)\\);\\s+"
                + "cache = JsonCache\\.Factory\\.instance\\.get\\(\"test-data\"\\)\\.load\\(cacheFile\\)\\.child\\(\"/org\\.openapitools\\.api/PetApi\"\\);";
        String reGetPetById = "(?s)(?m)public Pet getPetById\\(Long petId\\) \\{.*" // split
                + "try \\{\\s*" // split
                + "Pet response = cache\\.getObject\\(\"/getPetById/response\", Pet\\.class\\);";
        checkFile(generator, outputPath + "/src/main/java/org/openapitools/api/impl/PetApiServiceImpl.java", true,
                reInitCache, reGetPetById);

        reInitCache = "(?s)(?m)public static void beforeClass\\(\\) throws Exception \\{\\s+"
                + "File cacheFile = new File\\(System\\.getProperty\\(\"jaxrs\\.test\\.client\\.json\",\\s+"
                + "\".*src(?:\\\\\\\\|/)main(?:\\\\\\\\|/)resources(?:\\\\\\\\|/)test-data\\.json\"\\)\\);\\s+"
                + "cache = JsonCache\\.Factory\\.instance.get\\(\"test-data\"\\)\\.load\\(cacheFile\\)"
                + "\\.child\\(\"/org\\.openapitools\\.api/PetApi\"\\);";
        String reAddPetTest = "public void addPetTest\\(\\) throws Exception \\{\\s+"
                + "Pet pet = cache\\.getObject\\(\"/addPet/pet\", Pet\\.class\\);";
        checkFile(generator, outputPath + "/src/test/java/org/openapitools/api/PetApiTest.java", true, reInitCache,
                reAddPetTest);

        checkFile(generator, outputPath + "/src/main/resources/test-data.json", true);

        checkFile(generator, outputPath + "/test-data-control.json", true);
    }

    @Test
    public void testInitialConfigValues() throws Exception {
        codegen.processOpts();

        OpenAPI openAPI = new OpenAPI();
        openAPI.addServersItem(new Server().url("https://api.abcde.xy:8082/v2"));
        codegen.preprocessOpenAPI(openAPI);

        Map<String, Object> additionalProperties = codegen.additionalProperties();

        // Options processed by DefaultCodegen
        assertNull(additionalProperties.get(CodegenConstants.ALLOW_UNICODE_IDENTIFIERS));
        assertEquals(additionalProperties.get(CodegenConstants.API_PACKAGE), "org.openapitools.api");
        assertEquals(codegen.apiPackage(), "org.openapitools.api");
        assertNull(additionalProperties.get(CodegenConstants.DOCEXTENSION));
        assertNull(additionalProperties.get(CodegenConstants.ENSURE_UNIQUE_PARAMS));
        assertEquals(additionalProperties.get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        assertEquals(codegen.isHideGenerationTimestamp(), false);
        assertEquals(additionalProperties.get(CodegenConstants.INVOKER_PACKAGE), "org.openapitools.api");
        assertEquals(codegen.getInvokerPackage(), "org.openapitools.api");
        assertNull(additionalProperties.get(CodegenConstants.MODEL_NAME_PREFIX));
        assertNull(additionalProperties.get(CodegenConstants.MODEL_NAME_SUFFIX));
        assertEquals(additionalProperties.get(CodegenConstants.MODEL_PACKAGE), "org.openapitools.model");
        assertEquals(codegen.modelPackage(), "org.openapitools.model");
        assertNull(additionalProperties.get(CodegenConstants.PREPEND_FORM_OR_BODY_PARAMETERS));
        assertNull(additionalProperties.get(CodegenConstants.REMOVE_OPERATION_ID_PREFIX));
        assertNull(additionalProperties.get(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG));
        assertNull(additionalProperties.get(CodegenConstants.TEMPLATE_DIR));
        // Options processed by AbstractJavaCodegen
        assertEquals(additionalProperties.get(CodegenConstants.ARTIFACT_DESCRIPTION), "OpenAPI Java");
        assertEquals(additionalProperties.get(CodegenConstants.ARTIFACT_ID), "openapi-cxf-server");
        assertEquals(additionalProperties.get(CodegenConstants.ARTIFACT_VERSION), "1.0.0");
        assertEquals(additionalProperties.get(CodegenConstants.ARTIFACT_URL),
                "https://github.com/openapitools/openapi-generator");
        assertEquals(additionalProperties.get(CodegenConstants.DEVELOPER_EMAIL), "team@openapitools.org");
        assertEquals(additionalProperties.get(CodegenConstants.DEVELOPER_NAME), "OpenAPI-Generator Contributors");
        assertEquals(additionalProperties.get(CodegenConstants.DEVELOPER_ORGANIZATION), "OpenAPITools.org");
        assertEquals(additionalProperties.get(CodegenConstants.DEVELOPER_ORGANIZATION_URL), "http://openapitools.org");
        assertEquals(additionalProperties.get(CodegenConstants.GROUP_ID), "org.openapitools");
        assertEquals(additionalProperties.get(CodegenConstants.LICENSE_NAME), "Unlicense");
        assertEquals(additionalProperties.get(CodegenConstants.LICENSE_URL), "http://unlicense.org");
        assertEquals(additionalProperties.get(CodegenConstants.SCM_CONNECTION),
                "scm:git:git@github.com:openapitools/openapi-generator.git");
        assertEquals(additionalProperties.get(CodegenConstants.SCM_DEVELOPER_CONNECTION),
                "scm:git:git@github.com:openapitools/openapi-generator.git");
        assertEquals(additionalProperties.get(CodegenConstants.SCM_URL),
                "https://github.com/openapitools/openapi-generator");
        assertNull(additionalProperties.get(CodegenConstants.SERIALIZE_BIG_DECIMAL_AS_STRING));
        assertEquals(additionalProperties.get(CodegenConstants.SERIALIZABLE_MODEL), Boolean.FALSE);
        assertEquals(additionalProperties.get(CodegenConstants.SOURCE_FOLDER), "src/gen/java");
        assertEquals(additionalProperties.get(AbstractJavaCodegen.BOOLEAN_GETTER_PREFIX), "get");
        assertNull(additionalProperties.get(AbstractJavaCodegen.DATE_LIBRARY));
        assertEquals(additionalProperties.get(AbstractJavaCodegen.DISABLE_HTML_ESCAPING), Boolean.FALSE);
        assertEquals(additionalProperties.get(AbstractJavaCodegen.FULL_JAVA_UTIL), Boolean.FALSE);
        assertNull(additionalProperties.get(AbstractJavaCodegen.JAVA8_MODE));
        assertNull(additionalProperties.get(AbstractJavaCodegen.SUPPORT_ASYNC));
        assertEquals(additionalProperties.get(AbstractJavaCodegen.SUPPORT_JAVA6), Boolean.FALSE);
        assertEquals(additionalProperties.get(AbstractJavaCodegen.USE_NULL_FOR_UNKNOWN_ENUM_VALUE), false);
        assertEquals(additionalProperties.get(AbstractJavaCodegen.WITH_XML), false);
        // Options processed by AbstractJavaJAXRSServerCodegen
        assertNull(additionalProperties.get(CodegenConstants.IMPL_FOLDER));
        assertEquals(additionalProperties.get(BeanValidationFeatures.USE_BEANVALIDATION), Boolean.TRUE);
        assertEquals(additionalProperties.get(AbstractJavaJAXRSServerCodegen.SERVER_PORT), "8082");
        // Options processed by JavaCXFServerCodegen
        assertNull(additionalProperties.get(BeanValidationExtendedFeatures.USE_BEANVALIDATION_FEATURE));
        assertNull(additionalProperties.get(GzipFeatures.USE_GZIP_FEATURE));
        assertNull(additionalProperties.get(GzipTestFeatures.USE_GZIP_FEATURE_FOR_TESTS));
        assertNull(additionalProperties.get(JbossFeature.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR));
        assertNull(additionalProperties.get(LoggingFeatures.USE_LOGGING_FEATURE));
        assertNull(additionalProperties.get(LoggingTestFeatures.USE_LOGGING_FEATURE_FOR_TESTS));
        assertNull(additionalProperties.get(SpringFeatures.GENERATE_SPRING_APPLICATION));
        assertNull(additionalProperties.get(SpringFeatures.GENERATE_SPRING_BOOT_APPLICATION));
        assertNull(additionalProperties.get(SpringFeatures.USE_SPRING_ANNOTATION_CONFIG));
        assertNull(additionalProperties.get(SwaggerFeatures.USE_SWAGGER_FEATURE));
        assertNull(additionalProperties.get(SwaggerUIFeatures.USE_SWAGGER_UI));
        assertNull(additionalProperties.get(UseGenericResponseFeatures.USE_GENERIC_RESPONSE));
        assertNull(additionalProperties.get(CXFServerFeatures.ADD_CONSUMES_PRODUCES_JSON));
        assertNull(additionalProperties.get(CXFServerFeatures.GENERATE_NON_SPRING_APPLICATION));
        assertNull(additionalProperties.get(CXFExtServerFeatures.GENERATE_OPERATION_BODY));
        assertNull(additionalProperties.get(CXFServerFeatures.LOAD_TEST_DATA_FROM_FILE));
        assertNull(additionalProperties.get(CXFExtServerFeatures.SUPPORT_MULTIPLE_SPRING_SERVICES));
        assertNull(additionalProperties.get(CXFExtServerFeatures.TEST_DATA_FILE));
        assertNull(additionalProperties.get(CXFExtServerFeatures.TEST_DATA_CONTROL_FILE));
        assertNull(additionalProperties.get(CXFServerFeatures.USE_ANNOTATED_BASE_PATH));
        assertNull(additionalProperties.get(CXFServerFeatures.USE_MULTIPART_FEATURE));
        assertNull(additionalProperties.get(CXFServerFeatures.USE_WADL_FEATURE));
    }

    @Test
    public void testSettersForConfigValues() throws Exception {
        // It's apparent that most of these setters aren't useful to client code, only to the generator itself. The only
        // reliable way to set most features is through the additional properties, since CodegenConfig.processOpts()
        // overrides are typically coded to set config fields from the additional properties, not the other way round.
        codegen.setHideGenerationTimestamp(false);
        codegen.setModelPackage("xx.yyyyyyyy.model");
        codegen.setApiPackage("xx.yyyyyyyy.api");
        codegen.setInvokerPackage("xx.yyyyyyyy.invoker");
        codegen.processOpts();

        Map<String, Object> additionalProperties = codegen.additionalProperties();
        assertEquals(additionalProperties.get(CodegenConstants.HIDE_GENERATION_TIMESTAMP), Boolean.FALSE);
        assertEquals(codegen.isHideGenerationTimestamp(), false);
        assertEquals(codegen.modelPackage(), "xx.yyyyyyyy.model");
        assertEquals(additionalProperties.get(CodegenConstants.MODEL_PACKAGE), "xx.yyyyyyyy.model");
        assertEquals(codegen.apiPackage(), "xx.yyyyyyyy.api");
        assertEquals(additionalProperties.get(CodegenConstants.API_PACKAGE), "xx.yyyyyyyy.api");
        assertEquals(codegen.getInvokerPackage(), "xx.yyyyyyyy.invoker");
    }
}
