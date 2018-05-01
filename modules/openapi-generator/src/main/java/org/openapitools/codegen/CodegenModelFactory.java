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

package org.openapitools.codegen;

import java.util.HashMap;
import java.util.Map;

public final class CodegenModelFactory {

    private static final Map<CodegenModelType, Class<?>> typeMapping = new HashMap<CodegenModelType, Class<?>>();

    /**
     * Configure a different implementation class.
     *
     * @param type           the type that shall be replaced
     * @param implementation the implementation class must extend the default class and must provide a public no-arg constructor
     */
    public static void setTypeMapping(CodegenModelType type, Class<?> implementation) {
        if (!type.getDefaultImplementation().isAssignableFrom(implementation)) {
            throw new IllegalArgumentException(implementation.getSimpleName() + " doesn't extend " + type.getDefaultImplementation().getSimpleName());
        }
        try {
            implementation.newInstance();
        } catch (Exception e) {
            throw new IllegalArgumentException(e);
        }
        typeMapping.put(type, implementation);
    }

    @SuppressWarnings("unchecked")
    public static <T> T newInstance(CodegenModelType type) {
        Class<?> classType = typeMapping.get(type);
        try {
            return (T) (classType != null ? classType : type.getDefaultImplementation()).newInstance();
        } catch (IllegalAccessException | InstantiationException e) {
            throw new RuntimeException(e);
        }
    }
}
