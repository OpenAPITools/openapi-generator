package io.swagger.codegen.lumen;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.LumenServerCodegen;
import io.swagger.codegen.options.LumenServerOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class LumenServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private LumenServerCodegen clientCodegen;

    public LumenServerOptionsTest() {
        super(new LumenServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(LumenServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setParameterNamingConvention(LumenServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            clientCodegen.setModelPackage(LumenServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(LumenServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            times = 1;
            clientCodegen.setInvokerPackage(LumenServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackagePath(LumenServerOptionsProvider.PACKAGE_PATH_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(LumenServerOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setGitUserId(LumenServerOptionsProvider.GIT_USER_ID_VALUE);
            times = 1;
            clientCodegen.setGitRepoId(LumenServerOptionsProvider.GIT_REPO_ID_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(LumenServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
        }};
    }
}
