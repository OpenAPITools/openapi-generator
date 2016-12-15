package io.swagger.codegen.java;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.options.JavaClientOptionsProvider;
import io.swagger.codegen.options.JavaClientOptionsProvider;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.codegen.options.OptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class JavaClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private JavaClientCodegen clientCodegen;

    public JavaClientOptionsTest() {
        super(new JavaClientOptionsProvider());
    }

    protected JavaClientOptionsTest(OptionsProvider optionsProvider) {
        super(optionsProvider);
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(JavaClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(JavaClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(JavaClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(JavaClientOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(JavaClientOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(JavaClientOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(JavaClientOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setLicenseName(JavaClientOptionsProvider.LICENSE_NAME_VALUE);
            times = 1;
            clientCodegen.setLicenseUrl(JavaClientOptionsProvider.LICENSE_URL_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(JavaClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(JavaClientOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(JavaClientOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setLibrary(JavaClientOptionsProvider.DEFAULT_LIBRARY_VALUE);
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(JavaClientOptionsProvider.FULL_JAVA_UTIL_VALUE));
            times = 1;
            clientCodegen.setUseBeanValidation(Boolean.valueOf(JavaClientOptionsProvider.USE_BEANVALIDATION));
            times = 1;
            clientCodegen.setUseBeanValidation(Boolean.valueOf(JavaClientOptionsProvider.PERFORM_BEANVALIDATION));
            times = 1;
        }};
    }
}
