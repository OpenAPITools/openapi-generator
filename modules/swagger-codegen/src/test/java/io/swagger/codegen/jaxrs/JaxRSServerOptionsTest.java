package io.swagger.codegen.jaxrs;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.java.JavaClientOptionsTest;
import io.swagger.codegen.languages.JaxRSServerCodegen;

import mockit.Expectations;
import mockit.Tested;

public class JaxRSServerOptionsTest extends JavaClientOptionsTest {

    @Tested
    private JaxRSServerCodegen clientCodegen;

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setLocalVariablePrefix(LOCAL_PREFIX_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setLibrary(LIBRARY_VALUE);
            times = 1;
            clientCodegen.setFullJavaUtil(Boolean.valueOf(FULL_JAVA_UTIL_VALUE));
            times = 1;
        }};
    }
}
