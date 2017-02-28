package io.swagger.codegen.jaxrs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.JavaResteasyEapServerCodegen;
import io.swagger.codegen.options.JavaResteasyEapServerOptionsProvider;
import io.swagger.codegen.options.OptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class JavaResteasyEapServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private JavaResteasyEapServerCodegen clientCodegen;

    public JavaResteasyEapServerOptionsTest() {
        super(new JavaResteasyEapServerOptionsProvider());
    }

    protected JavaResteasyEapServerOptionsTest(OptionsProvider optionsProvider) {
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
                clientCodegen.setModelPackage(JavaResteasyEapServerOptionsProvider.MODEL_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setApiPackage(JavaResteasyEapServerOptionsProvider.API_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setSortParamsByRequiredFlag(
                        Boolean.valueOf(JavaResteasyEapServerOptionsProvider.SORT_PARAMS_VALUE));
                times = 1;
                clientCodegen.setInvokerPackage(JavaResteasyEapServerOptionsProvider.INVOKER_PACKAGE_VALUE);
                times = 1;
                clientCodegen.setGroupId(JavaResteasyEapServerOptionsProvider.GROUP_ID_VALUE);
                times = 1;
                clientCodegen.setArtifactId(JavaResteasyEapServerOptionsProvider.ARTIFACT_ID_VALUE);
                times = 1;
                clientCodegen.setArtifactVersion(JavaResteasyEapServerOptionsProvider.ARTIFACT_VERSION_VALUE);
                times = 1;
                clientCodegen.setSourceFolder(JavaResteasyEapServerOptionsProvider.SOURCE_FOLDER_VALUE);
                times = 1;
                clientCodegen.setLocalVariablePrefix(JavaResteasyEapServerOptionsProvider.LOCAL_PREFIX_VALUE);
                times = 1;
                clientCodegen.setSerializableModel(
                        Boolean.valueOf(JavaResteasyEapServerOptionsProvider.SERIALIZABLE_MODEL_VALUE));
                times = 1;
                clientCodegen
                        .setFullJavaUtil(Boolean.valueOf(JavaResteasyEapServerOptionsProvider.FULL_JAVA_UTIL_VALUE));
                times = 1;
                clientCodegen.setSerializeBigDecimalAsString(true);
                times = 1;

                clientCodegen.setGenerateJbossDeploymentDescriptor(
                        Boolean.valueOf(JavaResteasyEapServerOptionsProvider.GENERATE_JBOSS_DEPLOYMENT_DESCRIPTOR));
                times = 1;

                // no invocation as it is already defined as true in JavaResteasyEapServerCodegen
                // clientCodegen
                // .setUseBeanValidation(Boolean.valueOf(JavaResteasyEapServerOptionsProvider.USE_BEANVALIDATION));
                // times = 1;
                clientCodegen
                        .setUseSwaggerFeature(
                                Boolean.valueOf(JavaResteasyEapServerOptionsProvider.USE_SWAGGER_FEATURE));
                times = 1;

            }
        };
    }
}
