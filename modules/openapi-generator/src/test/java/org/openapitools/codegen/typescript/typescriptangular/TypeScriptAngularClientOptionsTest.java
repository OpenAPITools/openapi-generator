/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.typescript.typescriptangular;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.TypeScriptAngularClientCodegen;
import org.openapitools.codegen.options.TypeScriptAngularClientOptionsProvider;

public class TypeScriptAngularClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private TypeScriptAngularClientCodegen clientCodegen;

    public TypeScriptAngularClientOptionsTest() {
        super(new TypeScriptAngularClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setModelPropertyNaming(TypeScriptAngularClientOptionsProvider.MODEL_PROPERTY_NAMING_VALUE);
            times = 1;
            clientCodegen.setSupportsES6(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.SUPPORTS_ES6_VALUE));
            times = 1;
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(TypeScriptAngularClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
        }};
    }
}
