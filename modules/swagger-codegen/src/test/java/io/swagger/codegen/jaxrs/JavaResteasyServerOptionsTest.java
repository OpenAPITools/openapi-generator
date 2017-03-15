package io.swagger.codegen.jaxrs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.JavaResteasyServerCodegen;
import io.swagger.codegen.options.JavaResteasyServerOptionsProvider;
import io.swagger.codegen.options.OptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class JavaResteasyServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private JavaResteasyServerCodegen clientCodegen;

    public JavaResteasyServerOptionsTest() {
        super(new JavaResteasyServerOptionsProvider());
    }

    protected JavaResteasyServerOptionsTest(OptionsProvider optionsProvider) {
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
                clientCodegen.setModelPackage(JavaResteasyServerOptionsProvider.MODEL_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setApiPackage(JavaResteasyServerOptionsProvider.API_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setSortParamsByRequiredFlag(
                        Boolean.valueOf(JavaResteasyServerOptionsProvider.SORT_PARAMS_VALUE));
                times = 1;
                clientCodegen.setInvokerPackage(JavaResteasyServerOptionsProvider.INVOKER_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setGroupId(JavaResteasyServerOptionsProvider.GROUP_ID_VALUE);
                times = 1;
                clientCodegen.setArtifactId(JavaResteasyServerOptionsProvider.ARTIFACT_ID_VALUE);
                times = 1;
                clientCodegen.setArtifactVersion(JavaResteasyServerOptionsProvider.ARTIFACT_VERSION_VALUE);
                times = 1;
                clientCodegen.setSourceFolder(JavaResteasyServerOptionsProvider.SOURCE_FOLDER_VALUE);
                times = 1;
                clientCodegen.setLocalVariablePrefix(JavaResteasyServerOptionsProvider.LOCAL_PREFIX_VALUE);
                times = 1;
                clientCodegen.setSerializableModel(
                        Boolean.valueOf(JavaResteasyServerOptionsProvider.SERIALIZABLE_MODEL_VALUE));
                times = 1;
                clientCodegen.setFullJavaUtil(Boolean.valueOf(JavaResteasyServerOptionsProvider.FULL_JAVA_UTIL_VALUE));
                times = 1;
                clientCodegen.setSerializeBigDecimalAsString(true);
                times = 1;

                clientCodegen.setGenerateJbossDeploymentDescriptor(
                        Boolean.valueOf(JavaResteasyServerOptionsProvider.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR));
                
                clientCodegen.setUseBeanValidation(Boolean.valueOf(JavaResteasyServerOptionsProvider.USE_BEANVALIDATION));
                times = 1;

            }
        };
    }
}
