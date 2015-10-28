package io.swagger.codegen.springmvc;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.java.JavaClientOptionsTest;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.codegen.languages.SpringMVCServerCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.HashMap;
import java.util.Map;

public class SpringMVCServerOptionsTest extends JavaClientOptionsTest {
    protected static final String CONFIG_PACKAGE_VALUE = "configPackage";

    @Tested
    private SpringMVCServerCodegen clientCodegen;

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
            clientCodegen.setConfigPackage(CONFIG_PACKAGE_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        Map<String, String> options = new HashMap<String, String>(super.getAvaliableOptions());
        options.put(SpringMVCServerCodegen.CONFIG_PACKAGE, CONFIG_PACKAGE_VALUE);
        return options;
    }
}
