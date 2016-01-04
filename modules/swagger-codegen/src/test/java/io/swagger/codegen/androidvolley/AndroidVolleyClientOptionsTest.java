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
            clientCodegen.setModelPackage(AndroidVolleyClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(AndroidVolleyClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(AndroidVolleyClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setInvokerPackage(AndroidVolleyClientOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setGroupId(AndroidVolleyClientOptionsProvider.GROUP_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactId(AndroidVolleyClientOptionsProvider.ARTIFACT_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(AndroidVolleyClientOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setSourceFolder(AndroidVolleyClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setUseAndroidMavenGradlePlugin(Boolean.valueOf(AndroidVolleyClientOptionsProvider.ANDROID_MAVEN_GRADLE_PLUGIN_VALUE));
            times = 1;
        }};
    }
}
