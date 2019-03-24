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

package org.openapitools.codegen.python;

import mockit.Expectations;
import mockit.Tested;
import org.openapitools.codegen.AbstractOptionsTest;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.languages.PythonClientCodegen;
import org.openapitools.codegen.options.PythonClientOptionsProvider;

import java.io.File;

public class PythonClientOptionsTest extends AbstractOptionsTest {

    @Tested
    private PythonClientCodegen clientCodegen;

    public PythonClientOptionsTest() {
        super(new PythonClientOptionsProvider());
    }

    @Override
    protected CodegenConfig getCodegenConfig() {
        return clientCodegen;
    }

    @SuppressWarnings("unused")
    @Override
    protected void setExpectations() {
        new Expectations(clientCodegen) {{
            clientCodegen.setPackageName(PythonClientOptionsProvider.PACKAGE_NAME_VALUE);
            times = 1;

            clientCodegen.setProjectName(PythonClientOptionsProvider.PROJECT_NAME_VALUE);
            times = 1;

            clientCodegen.setPackageVersion(PythonClientOptionsProvider.PACKAGE_VERSION_VALUE);
            times = 1;

            clientCodegen.setPackageUrl(PythonClientOptionsProvider.PACKAGE_URL_VALUE);
            times = 1;

            clientCodegen.packagePath();
            result = PythonClientOptionsProvider.PACKAGE_NAME_VALUE.replace('.', File.separatorChar);
            minTimes = 1;
        }};
    }
}
