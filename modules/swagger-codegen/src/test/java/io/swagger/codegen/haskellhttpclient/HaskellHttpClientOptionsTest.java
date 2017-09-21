package io.swagger.codegen.haskellhttpclient;

import io.swagger.codegen.AbstractOptionsTest;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.languages.HaskellHttpClientCodegen;
import io.swagger.codegen.options.HaskellHttpClientOptionsProvider;
import mockit.Expectations;
import mockit.Tested;

public class HaskellHttpClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private HaskellHttpClientCodegen clientCodegen;

    public HaskellHttpClientOptionsTest() {
        super(new HaskellHttpClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(HaskellHttpClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(HaskellHttpClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(HaskellHttpClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;

            clientCodegen.setAllowFromJsonNulls(Boolean.valueOf(HaskellHttpClientOptionsProvider.ALLOW_FROMJSON_NULLS));
            times = 1;
            clientCodegen.setAllowToJsonNulls(Boolean.valueOf(HaskellHttpClientOptionsProvider.ALLOW_TOJSON_NULLS));
            times = 1;
            clientCodegen.setGenerateModelConstructors(Boolean.valueOf(HaskellHttpClientOptionsProvider.GENERATE_MODEL_CONSTRUCTORS));
            times = 1;
            clientCodegen.setGenerateFormUrlEncodedInstances(Boolean.valueOf(HaskellHttpClientOptionsProvider.GENERATE_FORM_URLENCODED_INSTANCES));
            times = 1;
            clientCodegen.setGenerateLenses(Boolean.valueOf(HaskellHttpClientOptionsProvider.GENERATE_LENSES));
            times = 1;
            clientCodegen.setModelDeriving(HaskellHttpClientOptionsProvider.MODEL_DERIVING);
            times = 1;
            clientCodegen.setDateTimeFormat(HaskellHttpClientOptionsProvider.DATETIME_FORMAT);
            times = 1;
            clientCodegen.setDateFormat(HaskellHttpClientOptionsProvider.DATE_FORMAT);
            times = 1;
            clientCodegen.setStrictFields(Boolean.valueOf(HaskellHttpClientOptionsProvider.STRICT_FIELDS));
            times = 1;
            clientCodegen.setUseMonadLogger(Boolean.valueOf(HaskellHttpClientOptionsProvider.USE_MONAD_LOGGER));
            times = 1;

        }};
    }
}
