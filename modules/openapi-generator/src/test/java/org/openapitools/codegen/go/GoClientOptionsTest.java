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

package org.openapitools.codegen.go;

import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.GoClientCodegen;
import org.openapitools.codegen.options.GoClientOptionsProvider;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public class GoClientOptionsTest extends AbstractOptionsTest {

    private GoClientCodegen clientCodegen = mock(GoClientCodegen.class, mockSettings);

    public GoClientOptionsTest() {
        super(new GoClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void verifyOptions() {
        verify(clientCodegen).setPackageVersion(GoClientOptionsProvider.PACKAGE_VERSION_VALUE);
        verify(clientCodegen).setPackageName(GoClientOptionsProvider.PACKAGE_NAME_VALUE);
        verify(clientCodegen).setWithXml(GoClientOptionsProvider.WITH_XML_VALUE);
        verify(clientCodegen).setEnumClassPrefix(GoClientOptionsProvider.ENUM_CLASS_PREFIX_VALUE);
        verify(clientCodegen).setPrependFormOrBodyParameters(GoClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE);
        verify(clientCodegen).setIsGoSubmodule(GoClientOptionsProvider.IS_GO_SUBMODULE_VALUE);
        verify(clientCodegen).setGenerateInterfaces(GoClientOptionsProvider.GENERATE_INTERFACES_VALUE);
        verify(clientCodegen).setStructPrefix(GoClientOptionsProvider.STRUCT_PREFIX_VALUE);
        verify(clientCodegen).setWithAWSV4Signature(GoClientOptionsProvider.WITH_AWSV4_SIGNATURE);
        verify(clientCodegen).setUseOneOfDiscriminatorLookup(GoClientOptionsProvider.USE_ONE_OF_DISCRIMINATOR_LOOKUP_VALUE);
    }
}
