package io.swagger.codegen.androidvolley;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.AndroidVolleyClientCodegen;
import io.swagger.codegen.options.AndroidClientOptionsProvider;
import io.swagger.codegen.options.AndroidVolleyClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class AndroidVolleyClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private AndroidVolleyClientCodegen clientCodegen;

    public AndroidVolleyClientOptionsTest() {
        super(new AndroidVolleyClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(AndroidClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(AndroidClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(AndroidClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(AndroidClientOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(AndroidClientOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(AndroidClientOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(AndroidClientOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(AndroidClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setUseAndroidMavenGradlePlugin(Boolean.valueOf(AndroidClientOptionsProvider.ANDROID_MAVEN_GRADLE_PLUGIN_VALUE));
            times = 1;
        }};
    }
}
