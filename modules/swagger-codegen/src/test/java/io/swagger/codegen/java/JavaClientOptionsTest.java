package io.swagger.codegen.java;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.options.JavaClientOptionsProvider;
import io.swagger.codegen.options.JavaOptionsProvider;
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
            clientCodegen.setModelPackage(JavaOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(JavaOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(JavaOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(JavaOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(JavaOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(JavaOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(JavaOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(JavaOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(JavaOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(JavaOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setLibrary(JavaClientOptionsProvider.DEFAULT_LIBRARY_VALUE);
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(JavaOptionsProvider.FULL_JAVA_UTIL_VALUE));
            times = 1;
            //clientCodegen.setSupportJava6(Boolean.valueOf(JavaOptionsProvider.SUPPORT_JAVA6));
            //times = 1;
            clientCodegen.setUseBeanValidation(Boolean.valueOf(JavaOptionsProvider.USE_BEANVALIDATION));
            times = 1;
        }};
    }
}
