/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

/**
 * The Context used for generation.
 *
 * @param <TSpecDocument> the type of the input spec document.
 */
public class Context<TSpecDocument> {
    private TSpecDocument specDocument;
    private GeneratorSettings settings;

    /**
     * Instantiates a new Context.
     *
     * @param specDocument the spec document
     * @param settings     the generator settings
     */
    public Context(TSpecDocument specDocument, GeneratorSettings settings) {
        this.specDocument = specDocument;
        this.settings = settings;
    }

    /**
     * Gets the generator settings.
     *
     * @return the settings
     */
    public GeneratorSettings getSettings() {
        return settings;
    }

    /**
     * Gets the spec document.
     *
     * @return the spec document
     */
    public TSpecDocument getSpecDocument() {
        return specDocument;
    }
}
