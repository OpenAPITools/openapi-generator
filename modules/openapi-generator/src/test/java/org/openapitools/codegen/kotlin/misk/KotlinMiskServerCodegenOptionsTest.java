package org.openapitools.codegen.kotlin.misk;

import java.util.List;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.KotlinMiskServerCodegen;
import org.openapitools.codegen.options.KotlinMiskServerCodegenOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class KotlinMiskServerCodegenOptionsTest extends AbstractOptionsTest {
    private KotlinMiskServerCodegen codegen = mock(KotlinMiskServerCodegen.class, mockSettings);

    public KotlinMiskServerCodegenOptionsTest() {
        super(new KotlinMiskServerCodegenOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return codegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(codegen).setPackageName(KotlinMiskServerCodegenOptionsProvider.PACKAGE_NAME_VALUE);
        verify(codegen).setGroupId(KotlinMiskServerCodegenOptionsProvider.GROUP_ID_VALUE);
        verify(codegen).setArtifactId(KotlinMiskServerCodegenOptionsProvider.ARTIFACT_ID_VALUE);
        verify(codegen).setArtifactVersion(KotlinMiskServerCodegenOptionsProvider.ARTIFACT_VERSION_VALUE);
        verify(codegen).setSourceFolder(KotlinMiskServerCodegenOptionsProvider.SOURCE_FOLDER_VALUE);
        verify(codegen).setSortParamsByRequiredFlag(Boolean.valueOf(KotlinMiskServerCodegenOptionsProvider.SORT_PARAMS_VALUE));
        verify(codegen).setSortModelPropertiesByRequiredFlag(Boolean.valueOf(KotlinMiskServerCodegenOptionsProvider.SORT_MODEL_PROPERTIES_VALUE));
        verify(codegen).setEnumPropertyNaming(KotlinMiskServerCodegenOptionsProvider.ENUM_PROPERTY_NAMING_VALUE);
        verify(codegen).setSerializableModel(Boolean.valueOf(KotlinMiskServerCodegenOptionsProvider.SERIALIZABLE_MODEL_VALUE));
        verify(codegen).setParcelizeModels(Boolean.valueOf(KotlinMiskServerCodegenOptionsProvider.PARCELIZE_MODELS_VALUE));
        verify(codegen).setApiSuffix(KotlinMiskServerCodegenOptionsProvider.API_SUFFIX_VALUE);
        verify(codegen).setAdditionalModelTypeAnnotations(List.of(KotlinMiskServerCodegenOptionsProvider.ADDITIONAL_MODEL_TYPE_ANNOTATIONS_VALUE));
        verify(codegen).setUseBeanValidation(Boolean.valueOf(KotlinMiskServerCodegenOptionsProvider.USE_BEAN_VALIDATION));
        verify(codegen).setModuleClassName(KotlinMiskServerCodegenOptionsProvider.MODULE_CLASS_NAME);
    }
}