/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.languages;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PythonClientExperimentalCodegen extends PythonClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonClientExperimentalCodegen.class);

    public PythonClientExperimentalCodegen() {
        super();
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-experimental";
    }
}
