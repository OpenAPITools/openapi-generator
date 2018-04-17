package org.openapitools.codegen.lumen;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PhpLumenServerCodegen;
import org.openapitools.codegen.options.PhpLumenServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class PhpLumenServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private PhpLumenServerCodegen clientCodegen;

    public PhpLumenServerOptionsTest() {
        super(new PhpLumenServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(PhpLumenServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setParameterNamingConvention(PhpLumenServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            clientCodegen.setModelPackage(PhpLumenServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(PhpLumenServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            times = 1;
            clientCodegen.setInvokerPackage(PhpLumenServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackagePath(PhpLumenServerOptionsProvider.PACKAGE_PATH_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(PhpLumenServerOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setGitUserId(PhpLumenServerOptionsProvider.GIT_USER_ID_VALUE);
            times = 1;
            clientCodegen.setGitRepoId(PhpLumenServerOptionsProvider.GIT_REPO_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(PhpLumenServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
        }};
    }
}
