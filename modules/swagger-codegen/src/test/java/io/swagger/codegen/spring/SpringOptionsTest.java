package io.swagger.codegen.spring;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.java.JavaClientOptionsTest;
import io.swagger.codegen.languages.SpringCodegen;
import io.swagger.codegen.options.SpringOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class SpringOptionsTest extends JavaClientOptionsTest {

    @Tested
    private SpringCodegen clientCodegen;

    public SpringOptionsTest() {
        super(new SpringOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(SpringOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(SpringOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SpringOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(SpringOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(SpringOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(SpringOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(SpringOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(SpringOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(SpringOptionsProvider.LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(SpringOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setLibrary(SpringOptionsProvider.LIBRARY_VALUE);
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(SpringOptionsProvider.FULL_JAVA_UTIL_VALUE));
            times = 1;
            clientCodegen.setTitle(SpringOptionsProvider.TITLE);
            times = 1;
            clientCodegen.setConfigPackage(SpringOptionsProvider.CONFIG_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setBasePackage(SpringOptionsProvider.BASE_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setInterfaceOnly(Boolean.valueOf(SpringOptionsProvider.INTERFACE_ONLY));
            times = 1;
            clientCodegen.setSingleContentTypes(Boolean.valueOf(SpringOptionsProvider.SINGLE_CONTENT_TYPES));
            times = 1;
            clientCodegen.setJava8(Boolean.valueOf(SpringOptionsProvider.JAVA_8));
            times = 1;
            clientCodegen.setAsync(Boolean.valueOf(SpringOptionsProvider.ASYNC));
            times = 1;
            clientCodegen.setResponseWrapper(SpringOptionsProvider.RESPONSE_WRAPPER);
            times = 1;

        }};
    }
}
