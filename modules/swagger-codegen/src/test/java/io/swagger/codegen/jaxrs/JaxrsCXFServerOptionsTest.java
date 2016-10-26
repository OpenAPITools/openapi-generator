package io.swagger.codegen.jaxrs;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.JavaCXFServerCodegen;
import io.swagger.codegen.options.JavaCXFServerOptionsProvider;
import io.swagger.codegen.options.OptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class JaxrsCXFServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private JavaCXFServerCodegen clientCodegen;

    public JaxrsCXFServerOptionsTest() {
        super(new JavaCXFServerOptionsProvider());
    }

    protected JaxrsCXFServerOptionsTest(OptionsProvider optionsProvider) {
        super(optionsProvider);
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(JavaCXFServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(JavaCXFServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(JavaCXFServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(JavaCXFServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(JavaCXFServerOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(JavaCXFServerOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(JavaCXFServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(JavaCXFServerOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(JavaCXFServerOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(JavaCXFServerOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(JavaCXFServerOptionsProvider.FULL_JAVA_UTIL_VALUE));
            times = 1;
            clientCodegen.setSerializeBigDecimalAsString(true);
            times = 1;
            
            clientCodegen.setUseBeanValidation(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_BEANVALIDATION));
            times = 1;
            
            clientCodegen.setGenerateSpringApplication(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_SWAGGER_FEATURE));
            times = 1;
            clientCodegen.setUseWadlFeature(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_WADL_FEATURE));
            times = 1;
            clientCodegen.setUseMultipartFeature(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_MULTIPART_FEATURE));
            times = 1;
            clientCodegen.setUseGzipFeature(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_GZIP_FEATURE));
            times = 1;
            clientCodegen.setUseLoggingFeature(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_LOGGING_FEATURE));
            times = 1;
            clientCodegen.setUseBeanValidationFeature(Boolean.valueOf(JavaCXFServerOptionsProvider.USE_BEANVALIDATION_FEATURE));
            times = 1;
            
            clientCodegen.setGenerateSpringBootApplication(Boolean.valueOf(JavaCXFServerOptionsProvider.GENERATE_SPRING_BOOT_APPLICATION));
            times = 1;
            
            
            
        }};
    }
}
