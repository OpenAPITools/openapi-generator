package org.openapitools.codegen.motoko;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.MotokoClientCodegen;
import org.openapitools.codegen.options.MotokoClientCodegenOptionsProvider;

import static java.lang.Boolean.parseBoolean;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.openapitools.codegen.CodegenConstants.*;
import static org.openapitools.codegen.languages.MotokoClientCodegen.*;
import static org.openapitools.codegen.options.MotokoClientCodegenOptionsProvider.*;

public class MotokoClientCodegenOptionsTest extends AbstractOptionsTest {
    private MotokoClientCodegen codegen = mock(MotokoClientCodegen.class, mockSettings);

    public MotokoClientCodegenOptionsTest() {
        super(new MotokoClientCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(codegen).setProjectName(PROJECT_NAME_VALUE);
        verify(codegen).setSortParamsByRequiredFlag(parseBoolean(SORT_PARAMS_VALUE));
        verify(codegen).setSortModelPropertiesByRequiredFlag(parseBoolean(SORT_MODEL_PROPERTIES_VALUE));
        verify(codegen).setEnsureUniqueParams(parseBoolean(ENSURE_UNIQUE_PARAMS_VALUE));
        verify(codegen).setAllowUnicodeIdentifiers(parseBoolean(ALLOW_UNICODE_IDENTIFIERS_VALUE));
        verify(codegen).setPrependFormOrBodyParameters(parseBoolean(PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(codegen).setLegacyDiscriminatorBehavior(parseBoolean(LEGACY_DISCRIMINATOR_BEHAVIOR_VALUE));
        verify(codegen).setDisallowAdditionalPropertiesIfNotPresent(parseBoolean(DISALLOW_ADDITIONAL_PROPERTIES_IF_NOT_PRESENT_VALUE));
        verify(codegen).setEnumUnknownDefaultCase(parseBoolean(ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
    }
}

