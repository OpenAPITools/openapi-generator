package io.swagger.codegen.jaxrs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.JavaCXFClientCodegen;
import io.swagger.codegen.options.JavaCXFClientOptionsProvider;
import io.swagger.codegen.options.OptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class JaxrsCXFClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private JavaCXFClientCodegen clientCodegen;

    public JaxrsCXFClientOptionsTest() {
        super(new JavaCXFClientOptionsProvider());
    }

    protected JaxrsCXFClientOptionsTest(OptionsProvider optionsProvider) {
        super(optionsProvider);
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {
            {
                clientCodegen.setModelPackage(JavaCXFClientOptionsProvider.MODEL_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setApiPackage(JavaCXFClientOptionsProvider.API_PACKAGE_VALUE);
                times = 1;
                clientCodegen
                        .setSortParamsByRequiredFlag(Boolean.valueOf(JavaCXFClientOptionsProvider.SORT_PARAMS_VALUE));
                times = 1;
                clientCodegen.setInvokerPackage(JavaCXFClientOptionsProvider.INVOKER_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setGroupId(JavaCXFClientOptionsProvider.GROUP_ID_VALUE);
                times = 1;
                clientCodegen.setArtifactId(JavaCXFClientOptionsProvider.ARTIFACT_ID_VALUE);
                times = 1;
                clientCodegen.setArtifactVersion(JavaCXFClientOptionsProvider.ARTIFACT_VERSION_VALUE);
                times = 1;
                clientCodegen.setSourceFolder(JavaCXFClientOptionsProvider.SOURCE_FOLDER_VALUE);
                times = 1;
                clientCodegen.setLocalVariablePrefix(JavaCXFClientOptionsProvider.LOCAL_PREFIX_VALUE);
                times = 1;
                clientCodegen
                        .setSerializableModel(Boolean.valueOf(JavaCXFClientOptionsProvider.SERIALIZABLE_MODEL_VALUE));
                times = 1;
                clientCodegen.setFullJavaUtil(Boolean.valueOf(JavaCXFClientOptionsProvider.FULL_JAVA_UTIL_VALUE));
                times = 1;
                clientCodegen.setSerializeBigDecimalAsString(true);
                times = 1;

                clientCodegen.setUseBeanValidation(Boolean.valueOf(JavaCXFClientOptionsProvider.USE_BEANVALIDATION));
                times = 1;
                clientCodegen.setUseGenericResponse(Boolean.valueOf(JavaCXFClientOptionsProvider.USE_GENERIC_RESPONSE));
                times = 1;

                clientCodegen.setUseLoggingFeatureForTests(
                        Boolean.valueOf(JavaCXFClientOptionsProvider.USE_LOGGING_FEATURE_FOR_TESTS));
                times = 1;

            }
        };
    }
}
