package io.swagger.codegen.springboot;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.SpringBootServerCodegen;
import io.swagger.codegen.options.SpringBootServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SpringBootServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SpringBootServerCodegen clientCodegen;

    public SpringBootServerOptionsTest() {
        super(new SpringBootServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(SpringBootServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(SpringBootServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SpringBootServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(SpringBootServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(SpringBootServerOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(SpringBootServerOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(SpringBootServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(SpringBootServerOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(SpringBootServerOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(SpringBootServerOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setLibrary(SpringBootServerOptionsProvider.LIBRARY_VALUE);
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(SpringBootServerOptionsProvider.FULL_JAVA_UTIL_VALUE));
            times = 1;
            clientCodegen.setConfigPackage(SpringBootServerOptionsProvider.CONFIG_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setBasePackage(SpringBootServerOptionsProvider.BASE_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setInterfaceOnly(Boolean.valueOf(SpringBootServerOptionsProvider.INTERFACE_ONLY));
            times = 1;
            clientCodegen.setSingleContentTypes(Boolean.valueOf(SpringBootServerOptionsProvider.SINGLE_CONTENT_TYPES));
            times = 1;
            clientCodegen.setJava8(Boolean.valueOf(SpringBootServerOptionsProvider.JAVA_8));
            times = 1;
            clientCodegen.setAsync(Boolean.valueOf(SpringBootServerOptionsProvider.ASYNC));
            times = 1;

        }};
    }
}
