/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.plugin;

import org.apache.maven.plugin.testing.AbstractMojoTestCase;

import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * A base test class where we can add helper methods and whatnot
 */
public abstract class BaseTestCase extends AbstractMojoTestCase {
    protected Path getUnitTestDir() {
        return Paths.get(getBasedir(), "src", "test", "resources", "unit");
    }
}
