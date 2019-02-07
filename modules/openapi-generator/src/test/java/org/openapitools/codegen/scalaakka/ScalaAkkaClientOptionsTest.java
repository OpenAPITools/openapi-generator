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

package org.openapitools.codegen.scalaakka;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.ScalaAkkaClientCodegen;
import org.openapitools.codegen.options.ScalaAkkaClientOptionsProvider;

public class ScalaAkkaClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private ScalaAkkaClientCodegen clientCodegen;

    public ScalaAkkaClientOptionsTest() {
        super(new ScalaAkkaClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setModelPackage(ScalaAkkaClientOptionsProvider.MODEL_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setApiPackage(ScalaAkkaClientOptionsProvider.API_PACKAGE_VALUE);
            times = 1;
            clientCodegen.setSortParamsByRequiredFlag(Boolean.valueOf(ScalaAkkaClientOptionsProvider.SORT_PARAMS_VALUE));
            times = 1;
            clientCodegen.setSourceFolder(ScalaAkkaClientOptionsProvider.SOURCE_FOLDER_VALUE);
            times = 1;
            clientCodegen.setPrependFormOrBodyParameters(Boolean.valueOf(ScalaAkkaClientOptionsProvider.PREPEND_FORM_OR_BODY_PARAMETERS_VALUE));
            times = 1;
            clientCodegen.setMainPackage(ScalaAkkaClientOptionsProvider.MAIN_PACKAGE_VALUE);
            times = 1;
        }};
    }
}
