/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.api.TemplatingEngineAdapter;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TemplatingEngineLoader {
    private TemplatingEngineLoader() {
        throw new IllegalStateException("Utility class");
    }

    @SuppressWarnings("java:S112") // ignore java:S112 as generic RuntimeException is acceptable here
    public static TemplatingEngineAdapter byIdentifier(String id) {
        ClassLoader currentThreadClassLoader = Thread.currentThread().getContextClassLoader();

        ServiceLoader<TemplatingEngineAdapter> staticClassLoader = ServiceLoader.load(TemplatingEngineAdapter.class, TemplatingEngineLoader.class.getClassLoader());
        ServiceLoader<TemplatingEngineAdapter> threadClassLoader = ServiceLoader.load(TemplatingEngineAdapter.class, currentThreadClassLoader);

        // combine the contents of the two class loaders. this preserves the existing behavior while allowing
        // use of the current context class loader
        List<TemplatingEngineAdapter> services = Stream.concat(staticClassLoader.stream(), threadClassLoader.stream())
                                                    .filter(Objects::nonNull)
                                                    .map(ServiceLoader.Provider::get)
                                                    .collect(Collectors.toList());

        Set<String> identifiers = new LinkedHashSet<>();
        for (TemplatingEngineAdapter templatingEngineAdapter : services) {
            if (id.equals(templatingEngineAdapter.getIdentifier())) {
                return templatingEngineAdapter;
            }
            identifiers.add(templatingEngineAdapter.getIdentifier());
        }

        try {
            // Attempt to load skipping SPI
            return (TemplatingEngineAdapter) currentThreadClassLoader.loadClass(id).getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException(String.format(Locale.ROOT, "Couldn't load template engine adapter %s. Available options: %n%s", id, String.join("\n", identifiers)), e);
        }
    }
}
