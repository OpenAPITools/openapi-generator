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

package org.openapitools.codegen.config;

import java.util.Properties;

/**
 * GeneratorProperties encapsulates SystemProperties, since the codegen mechanism heavily relies on a stable,
 * non-changing System Property Basis. Using plain System.(get|set|clear)Property raises Race-Conditions in combination
 * with Code, that uses System.setProperties (e.g. maven-surefire-plugin).
 * 
 * @author gndrm
 * @since 2018
 */
public class GeneratorProperties {

    private static ThreadLocal<Properties> properties = new InheritableThreadLocal<Properties>() {
        @Override
        protected Properties initialValue() {
            return (Properties) System.getProperties().clone();
        };
    };

    public static String getProperty(String key, String defaultValue) {
        return properties.get().getProperty(key, defaultValue);
    }

    public static String getProperty(String key) {
        return properties.get().getProperty(key);
    }

    public static void setProperty(String key, String value) {
        properties.get().setProperty(key, value);
    }

    public static void clearProperty(String key) {
        properties.get().remove(key);
    }

    public static void reset() {
        properties.remove();
    }
}
