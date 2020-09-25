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

package org.openapitools.codegen.utils;

public class ImplementationVersion {
    public static String read() {
        // Assumes this version is required at runtime. This could be modified to use a properties file like the CLI.
        String compiledVersion = ImplementationVersion.class.getPackage().getImplementationVersion();
        if (compiledVersion != null) {
            return compiledVersion;
        }

        // When running non-JARed class within an IDE the implementation version is not available, so we provide a means
        // to set it externally via a system property so that generated artefacts contain the correct version.
        return System.getProperty("openapitools.implementation.version", "unset");
    }
}
