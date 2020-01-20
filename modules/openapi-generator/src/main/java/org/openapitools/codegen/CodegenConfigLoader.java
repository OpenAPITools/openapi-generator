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

package org.openapitools.codegen;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

public class CodegenConfigLoader {
    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    public static CodegenConfig forName(String name) {
        ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class, CodegenConfig.class.getClassLoader());

        StringBuilder availableConfigs = new StringBuilder();

        for (CodegenConfig config : loader) {
            if (config.getName().equals(name)) {
                return config;
            }

            availableConfigs.append(config.getName()).append("\n");
        }

        // else try to load directly
        try {
            return (CodegenConfig) Class.forName(name).getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new GeneratorNotFoundException("Can't load config class with name '".concat(name) + "'\nAvailable:\n" + availableConfigs.toString(), e);
        }
    }

    public static List<CodegenConfig> getAll() {
        ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class, CodegenConfig.class.getClassLoader());
        List<CodegenConfig> output = new ArrayList<CodegenConfig>();
        for (CodegenConfig aLoader : loader) {
            output.add(aLoader);
        }
        return output;
    }
}
