package org.openapitools.codegen.php;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpClientCodegen;
import org.openapitools.codegen.options.PhpClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class PhpClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpClientCodegen clientCodegen;

    public PhpClientOptionsTest() {
        super(new PhpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(PhpClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(PhpClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setParameterNamingConvention(PhpClientOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            times = 1;
            clientCodegen.setInvokerPackage(PhpClientOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackagePath(PhpClientOptionsProvider.PACKAGE_PATH_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(PhpClientOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setComposerVendorName(PhpClientOptionsProvider.COMPOSER_VENDOR_NAME_VALUE);
            times = 1;
            clientCodegen.setGitUserId(PhpClientOptionsProvider.GIT_USER_ID_VALUE);
            times = 1;
            clientCodegen.setComposerProjectName(PhpClientOptionsProvider.COMPOSER_PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setGitRepoId(PhpClientOptionsProvider.GIT_REPO_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(PhpClientOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
        }};
    }
}
