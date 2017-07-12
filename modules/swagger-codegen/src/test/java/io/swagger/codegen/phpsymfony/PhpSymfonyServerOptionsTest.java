package io.swagger.codegen.phpsymfony;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.AbstractPhpCodegen;
import io.swagger.codegen.languages.SymfonyServerCodegen;
import io.swagger.codegen.options.SymfonyServerOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class PhpSymfonyServerOptionsTest extends AbstractOptionsTest {

    @Tested
    private SymfonyServerCodegen symfonyCodegen;

    public PhpSymfonyServerOptionsTest() {
        super(new SymfonyServerOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return symfonyCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(symfonyCodegen) {{
            symfonyCodegen.setBundleName(SymfonyServerOptionsProvider.BUNDLE_NAME_VALUE);
            times = 1;
            symfonyCodegen.setModelPackage(SymfonyServerOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            symfonyCodegen.setApiPackage(SymfonyServerOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            symfonyCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(SymfonyServerOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            symfonyCodegen.setParameterNamingConvention(SymfonyServerOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            times = 1;
            symfonyCodegen.setInvokerPackage(SymfonyServerOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            symfonyCodegen.setPackagePath(SymfonyServerOptionsProvider.PACKAGE_PATH_VALUE);
            times = 1;
            symfonyCodegen.setSrcBasePath(SymfonyServerOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            symfonyCodegen.setComposerVendorName(SymfonyServerOptionsProvider.COMPOSER_VENDOR_NAME_VALUE);
            times = 1;
            symfonyCodegen.setGitUserId(SymfonyServerOptionsProvider.GIT_USER_ID_VALUE);
            times = 1;
            symfonyCodegen.setComposerProjectName(SymfonyServerOptionsProvider.COMPOSER_PROJECT_NAME_VALUE);
            times = 1;
            symfonyCodegen.setGitRepoId(SymfonyServerOptionsProvider.GIT_REPO_ID_VALUE);
            times = 1;
            symfonyCodegen.setArtifactVersion(SymfonyServerOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
        }};
    }
}
