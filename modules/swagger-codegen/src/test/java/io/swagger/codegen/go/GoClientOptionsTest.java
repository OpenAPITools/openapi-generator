package io.swagger.codegen.Go;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.GoClientCodegen;
import io.swagger.codegen.options.GoClientOptionsProvider;

import mockit.Expectations;
import mockit.Tested;

public class GoClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private GoClientCodegen clientCodegen;

    public GoClientOptionsTest() {
        super(new GoClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(GoClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(GoClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            //clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(GoClientOptionsProvider.SORT_PARAMS_VALUE));
            //times = 1;
            clientCodegen.setParameterNamingConvention(GoClientOptionsProvider.VARIABLE_NAMING_CONVENTION_VALUE);
            times = 1;
            clientCodegen.setInvokerPackage(GoClientOptionsProvider.INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackagePath(GoClientOptionsProvider.PACKAGE_PATH_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(GoClientOptionsProvider.SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setComposerVendorName(GoClientOptionsProvider.COMPOSER_VENDOR_NAME_VALUE);
            times = 1;
            clientCodegen.setComposerProjectName(GoClientOptionsProvider.COMPOSER_PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(GoClientOptionsProvider.ARTIFACT_VERSION_VALUE);
            times = 1;
            clientCodegen.setPackageVersion(GoClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;
            clientCodegen.setPackageName(GoClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;                        
        }};
    }
}
