package io.swagger.codegen.php;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.PhpClientCodegen;

import com.google.common.collect.ImmutableMap;
import mockit.Expectations;
import mockit.Tested;

import java.util.Map;

public class PhpClientOptionsTest extends AbstractOptionsTest {
    protected static final String MODEL_PACKAGE_VALUE = "package";
    protected static final String API_PACKAGE_VALUE = "apiPackage";
    protected static final String SORT_PARAMS_VALUE = "false";
    protected static final String VARIABLE_NAMING_CONVENTION_VALUE = "snake_case";
    protected static final String INVOKER_PACKAGE_VALUE = "Swagger\\Client\\Php";
    protected static final String PACKAGE_PATH_VALUE = "SwaggerClient-php";
    protected static final String SRC_BASE_PATH_VALUE = "libPhp";
    protected static final String COMPOSER_VENDOR_NAME_VALUE = "swaggerPhp";
    protected static final String COMPOSER_PROJECT_NAME_VALUE = "swagger-client-php";
    protected static final String ARTIFACT_VERSION_VALUE = "1.0.0-SNAPSHOT";

    @Tested
    private PhpClientCodegen clientCodegen;

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
            clientCodegen.setParameterNamingConvention(VARIABLE_NAMING_CONVENTION_VALUE);
            times = 1;
            clientCodegen.setInvokerPackage(INVOKER_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setPackagePath(PACKAGE_PATH_VALUE);
            times = 1;
            clientCodegen.setSrcBasePath(SRC_BASE_PATH_VALUE);
            times = 1;
            clientCodegen.setComposerVendorName(COMPOSER_VENDOR_NAME_VALUE);
            times = 1;
            clientCodegen.setComposerProjectName(COMPOSER_PROJECT_NAME_VALUE);
            times = 1;
            clientCodegen.setArtifactVersion(ARTIFACT_VERSION_VALUE);
            times = 1;
        }};
    }

    @Override
    protected Map<String, String> getAvaliableOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.MODEL_PACKAGE, MODEL_PACKAGE_VALUE)
                .put(CodegenConstants.API_PACKAGE, API_PACKAGE_VALUE)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_VALUE)
                .put(PhpClientCodegen.VARIABLE_NAMING_CONVENTION, VARIABLE_NAMING_CONVENTION_VALUE)
                .put(CodegenConstants.INVOKER_PACKAGE, INVOKER_PACKAGE_VALUE)
                .put(PhpClientCodegen.PACKAGE_PATH, PACKAGE_PATH_VALUE)
                .put(PhpClientCodegen.SRC_BASE_PATH, SRC_BASE_PATH_VALUE)
                .put(PhpClientCodegen.COMPOSER_VENDOR_NAME, COMPOSER_VENDOR_NAME_VALUE)
                .put(PhpClientCodegen.COMPOSER_PROJECT_NAME, COMPOSER_PROJECT_NAME_VALUE)
                .put(CodegenConstants.ARTIFACT_VERSION, ARTIFACT_VERSION_VALUE)
                .build();
    }
}
