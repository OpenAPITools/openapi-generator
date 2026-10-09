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

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import java.util.ServiceConfigurationError;
import java.util.Set;
import java.util.WeakHashMap;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CodegenConfigLoader {
    private static final Logger LOGGER = LoggerFactory.getLogger(CodegenConfigLoader.class);
    // Guarded entirely by explicit synchronization on the map below, so a plain WeakHashMap suffices.
    private static final Map<ClassLoader, Set<String>> INITIALIZATION_FAILURES = new WeakHashMap<>();
    private static final ThreadLocal<ClassLoader> LOADING_CLASS_LOADER = new ThreadLocal<>();

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
        } catch (ClassNotFoundException e) {
            throw generatorNotFoundException(name, availableConfigs, e);
        } catch (NoClassDefFoundError e) {
            if (e.getCause() instanceof ExceptionInInitializerError || hasInitializationFailed(name)) {
                throw generatorInitializationException(name, availableConfigs, e);
            }
            throw generatorNotFoundException(name, availableConfigs, e);
        } catch (UnsupportedClassVersionError e) {
            throw generatorIncompatibleException(name, availableConfigs, e);
        } catch (ExceptionInInitializerError e) {
            throw generatorInitializationException(name, availableConfigs, e);
        } catch (LinkageError e) {
            throw generatorLinkageException(name, availableConfigs, e);
        } catch (ReflectiveOperationException | ClassCastException e) {
            throw new GeneratorNotFoundException(
                    "Can't instantiate config class with name '" + name + "'. The class was found but could not be "
                            + "constructed; it must implement CodegenConfig, declare a public no-argument constructor, "
                            + "and that constructor must not throw.\nAvailable:\n" + availableConfigs, e);
        } finally {
            LOADING_CLASS_LOADER.remove();
        }
    }

    public static List<CodegenConfig> getAll() {
        List<CodegenConfig> output = new ArrayList<CodegenConfig>();
        Set<String> configClasses = new HashSet<String>();
        for (ClassLoader classLoader : getConfigClassLoaders()) {
            ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class, classLoader);
            Iterator<ServiceLoader.Provider<CodegenConfig>> providers = loader.stream().iterator();
            while (true) {
                ServiceLoader.Provider<CodegenConfig> provider;
                // Per-entry failures (missing/invalid provider class, LinkageError) happen after the
                // cursor advances, so skip them and keep discovering. A resource-location failure
                // (getResources IOException) doesn't advance and would loop forever: terminate instead.
                try {
                    if (!providers.hasNext()) {
                        break;
                    }
                    provider = providers.next();
                } catch (ServiceConfigurationError | LinkageError e) {
                    LOGGER.warn("Unable to enumerate codegen config provider from {}", classLoader, e);
                    if (isNonAdvancingResourceError(e)) {
                        break;
                    }
                    continue;
                }
                // Cursor has advanced past this provider, so loading/instantiation failures are safe to skip.
                try {
                    String configClassName = provider.type().getName();
                    if (!configClasses.contains(configClassName)) {
                        CodegenConfig config = provider.get();
                        configClasses.add(configClassName);
                        output.add(config);
                    }
                } catch (ServiceConfigurationError | LinkageError e) {
                    LOGGER.warn("Unable to load codegen config provider from {}", classLoader, e);
                }
            }
        }
        return output;
    }

    /**
     * Whether an iterator-advancement error is a non-advancing resource-location failure rather than
     * a single-entry failure the cursor has already moved past. Only the JDK's "Error locating
     * configuration files" case (getResources itself failing) leaves the cursor un-advanced and recurs
     * on every retry, so enumeration must stop; every other failure - including "Error reading
     * configuration file", which has already consumed a resource - can be skipped to keep discovering
     * the remaining valid providers.
     */
    private static boolean isNonAdvancingResourceError(Throwable e) {
        return e instanceof ServiceConfigurationError
                && e.getCause() instanceof IOException
                && e.getMessage() != null
                && e.getMessage().contains("Error locating configuration files");
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
            return loadConfigClass(className, classLoader);
        } catch (ClassNotFoundException ignored) {
            if (classLoader != CodegenConfig.class.getClassLoader()) {
                return loadConfigClass(className, CodegenConfig.class.getClassLoader());
            }
            throw ignored;
        }
    }

    private static Class<?> loadConfigClass(String className, ClassLoader classLoader) throws ClassNotFoundException {
        LOADING_CLASS_LOADER.set(classLoader);
        try {
            return Class.forName(className, true, classLoader);
        } catch (ExceptionInInitializerError e) {
            rememberInitializationFailure(className, classLoader);
            throw e;
        }
    }

    private static void rememberInitializationFailure(String name, ClassLoader classLoader) {
        synchronized (INITIALIZATION_FAILURES) {
            INITIALIZATION_FAILURES.computeIfAbsent(classLoader, ignored -> new HashSet<>()).add(name);
        }
    }

    private static boolean hasInitializationFailed(String name) {
        ClassLoader classLoader = LOADING_CLASS_LOADER.get();
        if (classLoader == null) {
            return false;
        }
        synchronized (INITIALIZATION_FAILURES) {
            Set<String> failures = INITIALIZATION_FAILURES.get(classLoader);
            return failures != null && failures.contains(name);
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

    private static GeneratorNotFoundException generatorIncompatibleException(String name,
                                                                              StringBuilder availableConfigs,
                                                                              Throwable cause) {
        return new GeneratorNotFoundException(
                "Can't load config class with name '" + name + "'. The class or one of its dependencies was compiled "
                        + "for an incompatible Java version. Use a generator compiled for the Java version running "
                        + "OpenAPI Generator.\nAvailable:\n" + availableConfigs, cause);
    }

    private static GeneratorNotFoundException generatorInitializationException(String name,
                                                                                StringBuilder availableConfigs,
                                                                                Throwable cause) {
        return new GeneratorNotFoundException(
                "Can't load config class with name '" + name + "'. The class was found but its static initializer "
                        + "failed; inspect the underlying exception for the cause.\nAvailable:\n" + availableConfigs, cause);
    }

    private static GeneratorNotFoundException generatorLinkageException(String name,
                                                                         StringBuilder availableConfigs,
                                                                         Throwable cause) {
        return new GeneratorNotFoundException(
                "Can't load config class with name '" + name + "'. The class was found but could not be linked "
                        + "(" + cause.getClass().getSimpleName() + "); inspect the underlying error for the cause.\nAvailable:\n"
                        + availableConfigs, cause);
    }
}
