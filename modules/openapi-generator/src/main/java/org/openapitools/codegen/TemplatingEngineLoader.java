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

import java.util.Locale;
import java.util.ServiceLoader;

public class TemplatingEngineLoader {
    public static TemplatingEngineAdapter byIdentifier(String id) {
        ServiceLoader<TemplatingEngineAdapter> loader = ServiceLoader.load(TemplatingEngineAdapter.class, TemplatingEngineLoader.class.getClassLoader());

        StringBuilder sb = new StringBuilder();
        for (TemplatingEngineAdapter templatingEngineAdapter : loader) {
            if (id.equals(templatingEngineAdapter.getIdentifier())) {
                return templatingEngineAdapter;
            }
            sb.append(templatingEngineAdapter.getIdentifier()).append("\n");
        }

        try {
            // Attempt to load skipping SPI
            return (TemplatingEngineAdapter) Class.forName(id).getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException(String.format(Locale.ROOT, "Couldn't load template engine adapter %s. Available options: \n%s", id, sb.toString()), e);
        }
    }
}
