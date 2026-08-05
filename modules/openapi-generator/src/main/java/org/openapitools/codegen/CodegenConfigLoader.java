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
import java.util.HashSet;
import java.util.List;
import java.util.ServiceLoader;
import java.util.Set;

public class CodegenConfigLoader {
    /**
     * Tries to load config class with SPI first, then with class name directly from classpath
     *
     * @param name name of config, or full qualified class name in classpath
     * @return config class
     */
    public static CodegenConfig forName(String name) {
        StringBuilder availableConfigs = new StringBuilder();

        for (CodegenConfig config : getAll()) {
            if (config.getName().equals(name)) {
                return config;
            }

            availableConfigs.append(config.getName()).append("\n");
        }

        // else try to load directly
        try {
            return loadConfigClass(name).asSubclass(CodegenConfig.class).getDeclaredConstructor().newInstance();
        } catch (ClassNotFoundException | LinkageError e) {
            throw generatorNotFoundException(name, availableConfigs, e);
        } catch (ReflectiveOperationException | ClassCastException e) {
            throw new GeneratorNotFoundException(
                    "Can't instantiate config class with name '" + name + "'. The class was found but could not be "
                            + "constructed; it must implement CodegenConfig, declare a public no-argument constructor, "
                            + "and that constructor must not throw.\nAvailable:\n" + availableConfigs, e);
        }
    }

    public static List<CodegenConfig> getAll() {
        List<CodegenConfig> output = new ArrayList<CodegenConfig>();
        Set<String> configClasses = new HashSet<String>();
        for (ClassLoader classLoader : getConfigClassLoaders()) {
            ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class, classLoader);
            for (CodegenConfig config : loader) {
                if (configClasses.add(config.getClass().getName())) {
                    output.add(config);
                }
            }
        }
        return output;
    }

    private static ClassLoader getConfigClassLoader() {
        ClassLoader contextClassLoader = Thread.currentThread().getContextClassLoader();
        return contextClassLoader != null ? contextClassLoader : CodegenConfig.class.getClassLoader();
    }

    private static List<ClassLoader> getConfigClassLoaders() {
        ClassLoader primaryClassLoader = getConfigClassLoader();
        ClassLoader definingClassLoader = CodegenConfig.class.getClassLoader();
        if (primaryClassLoader == definingClassLoader) {
            return List.of(definingClassLoader);
        }
        return List.of(primaryClassLoader, definingClassLoader);
    }

    private static Class<?> loadConfigClass(String className) throws ClassNotFoundException {
        ClassLoader classLoader = getConfigClassLoader();
        try {
            return Class.forName(className, true, classLoader);
        } catch (ClassNotFoundException ignored) {
            if (classLoader != CodegenConfig.class.getClassLoader()) {
                return Class.forName(className, true, CodegenConfig.class.getClassLoader());
            }
            throw ignored;
        }
    }

    private static GeneratorNotFoundException generatorNotFoundException(String name,
                                                                         StringBuilder availableConfigs,
                                                                         Throwable cause) {
        return new GeneratorNotFoundException(
                "Can't load config class with name '" + name + "'. The class or one of its dependencies could not "
                        + "be loaded from the generation runtime classpath. Ensure the class (and its dependencies) "
                        + "are on the classpath used to launch the generator.\nAvailable:\n" + availableConfigs, cause);
    }
}
