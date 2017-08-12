package io.swagger.codegen.jaxrs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.java.JavaClientOptionsTest;
import io.swagger.codegen.languages.JavaJerseyServerCodegen;
import io.swagger.codegen.options.JaxRSServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class JaxRSServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private JavaJerseyServerCodegen clientCodegen;

    public JaxRSServerOptionsTest() {
        super(new JaxRSServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(JaxRSServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(JaxRSServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(JaxRSServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(JaxRSServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(JaxRSServerOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(JaxRSServerOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(JaxRSServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setArtifactUrl(JaxRSServerOptionsProvider.ARTIFACT_URL_VALUE);
            times = 1;
            clientCodegen.setArtifactDescription(JaxRSServerOptionsProvider.ARTIFACT_DESCRIPTION_VALUE);
            times = 1;
            clientCodegen.setScmConnection(JaxRSServerOptionsProvider.SCM_CONNECTION_VALUE);
            times = 1;
            clientCodegen.setScmDeveloperConnection(JaxRSServerOptionsProvider.SCM_DEVELOPER_CONNECTION_VALUE);
            times = 1;
            clientCodegen.setScmUrl(JaxRSServerOptionsProvider.SCM_URL_VALUE);
            times = 1;
            clientCodegen.setDeveloperName(JaxRSServerOptionsProvider.DEVELOPER_NAME_VALUE);
            times = 1;
            clientCodegen.setDeveloperEmail(JaxRSServerOptionsProvider.DEVELOPER_EMAIL_VALUE);
            times = 1;
            clientCodegen.setDeveloperOrganization(JaxRSServerOptionsProvider.DEVELOPER_ORGANIZATION_VALUE);
            times = 1;
            clientCodegen.setDeveloperOrganizationUrl(JaxRSServerOptionsProvider.DEVELOPER_ORGANIZATION_URL_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(JaxRSServerOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(JaxRSServerOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(JaxRSServerOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            //clientCodegen.setLibrary(JaxRSServerOptionsProvider.JAXRS_LIBRARY_VALUE);
            clientCodegen.setLibrary("jersey1");
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(JaxRSServerOptionsProvider.FULL_JAVA_UTIL_VALUE));
            times = 1;
            clientCodegen.setDateLibrary("joda");
            times = 1;
            clientCodegen.setSupportJava6(false);
            times = 1;
            clientCodegen.setUseBeanValidation(Boolean.valueOf(JaxRSServerOptionsProvider.USE_BEANVALIDATION));
            times = 1;           
            clientCodegen.setUseTags(Boolean.valueOf(JaxRSServerOptionsProvider.USE_TAGS));
            times = 1;
        }};
    }
}
