/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.typescript.fetch;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptFetchClientCodegen;
import org.openapitools.codegen.options.TypeScriptFetchClientOptionsProvider;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.Assert;
import org.testng.annotations.Test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_FETCH})
public class TypeScriptFetchClientOptionsTest extends AbstractOptionsTest {
    private TypeScriptFetchClientCodegen clientCodegen = mock(TypeScriptFetchClientCodegen.class, mockSettings);

    public TypeScriptFetchClientOptionsTest() {
        super(new TypeScriptFetchClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.SORT_PARAMS_VALUE));
        verify(clientCodegen).setModelPropertyNaming(TypeScriptFetchClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
        verify(clientCodegen).setParamNaming(TypeScriptFetchClientOptionsProvider.PARAM_NAMING_VALUE);
        verify(clientCodegen).setSupportsES6(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.SUPPORTS_ES6_VALUE));
        verify(clientCodegen).setImportFileExtension(TypeScriptFetchClientOptionsProvider.IMPORT_FILE_EXTENSION_VALUE);
        verify(clientCodegen).setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
        verify(clientCodegen).setWithoutRuntimeChecks(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.WITHOUT_RUNTIME_CHECKS));
        verify(clientCodegen).setSagasAndRecords(Boolean.valueOf(TypeScriptFetchClientOptionsProvider.SAGAS_AND_RECORDS));
        verify(clientCodegen).setEnumUnknownDefaultCase(Boolean.parseBoolean(TypeScriptFetchClientOptionsProvider.ENUM_UNKNOWN_DEFAULT_CASE_VALUE));
        verify(clientCodegen).setStringEnums(Boolean.parseBoolean(TypeScriptFetchClientOptionsProvider.STRING_ENUMS));
        verify(clientCodegen).setFileNaming(TypeScriptFetchClientOptionsProvider.FILE_NAMING_VALUE);
    }

    @Test(description = "Verify if an exception is thrown when invalid values are used with fileNaming option")
    public void testFileNamingInvalidValues() {
        final TypeScriptFetchClientCodegen codegen = new TypeScriptFetchClientCodegen();
        Assert.assertThrows(IllegalArgumentException.class, () ->
                codegen.setFileNaming(null)
        );
        Assert.assertThrows(IllegalArgumentException.class, () ->
                codegen.setFileNaming("")
        );
        Assert.assertThrows(IllegalArgumentException.class, () ->
                codegen.setFileNaming("invalid-format")
        );
    }

}
