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

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CodegenConfigLoader {
    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    public static CodegenConfig forName(String name) {
        Set<String> availableConfigs = new LinkedHashSet<>();

        for (CodegenConfig config : getAll()) {
            if (config.getName().equals(name)) {
                return config;
            }

            availableConfigs.add(config.getName());
        }

        // else try to load directly
        try {
            return (CodegenConfig) Thread.currentThread().getContextClassLoader().loadClass(name).getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new GeneratorNotFoundException("Can't load config class with name '".concat(name) + "'\nAvailable:\n" + String.join("\n", availableConfigs), e);
        }
    }

    public static List<CodegenConfig> getAll() {
        ServiceLoader<CodegenConfig> staticClassLoader = ServiceLoader.load(CodegenConfig.class, CodegenConfig.class.getClassLoader());
        ServiceLoader<CodegenConfig> threadClassLoader = ServiceLoader.load(CodegenConfig.class, Thread.currentThread().getContextClassLoader());
        return  Stream.concat(staticClassLoader.stream(), threadClassLoader.stream())
                .filter(Objects::nonNull)
                .map(ServiceLoader.Provider::get)
                .collect(Collectors.toList());
    }
}
