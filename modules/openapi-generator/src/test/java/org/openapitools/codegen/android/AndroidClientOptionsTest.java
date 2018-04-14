package org.openapitools.codegen.android;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.AndroidClientCodegen;
import org.openapitools.codegen.options.AndroidClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class AndroidClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private AndroidClientCodegen clientCodegen;

    public AndroidClientOptionsTest() {
        super(new AndroidClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
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
            clientCodegen.setAndroidGradleVersion(AndroidClientOptionsProvider.ANDROID_GRADLE_VERSION_VALUE);
            times = 1;
            clientCodegen.setAndroidSdkVersion(AndroidClientOptionsProvider.ANDROID_SDK_VERSION_VALUE);
            times = 1;
            clientCodegen.setAndroidBuildToolsVersion(AndroidClientOptionsProvider.ANDROID_BUILD_TOOLS_VERSION_VALUE);
            times = 1;
            clientCodegen.setLibrary(AndroidClientOptionsProvider.LIBRARY_VALUE);
            times = 1;
            clientCodegen.setSerializableModel(Boolean.valueOf(AndroidClientOptionsProvider.SERIALIZABLE_MODEL_VALUE));
            times = 1;
            clientCodegen.setAllowUnicodeIdentifiers(Boolean.valueOf(AndroidClientOptionsProvider.ALLOW_UNICODE_IDENTIFIERS_VALUE));
            times = 1;
        }};
    }
}
