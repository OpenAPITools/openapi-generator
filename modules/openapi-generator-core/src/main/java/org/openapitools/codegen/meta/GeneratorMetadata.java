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

package org.openapitools.codegen.meta;

/**
 * Represents metadata about a generator.
 */
public class GeneratorMetadata {
    private Stability stability;
    private String generationMessage;

    private GeneratorMetadata(Builder builder) {
        stability = builder.stability;
        generationMessage = builder.generationMessage;
    }

    /**
     * Creates a new builder object for {@link GeneratorMetadata}.
     *
     * @return A new builder instance.
     */
    public static Builder newBuilder() {
        return new Builder();
    }

    /**
     * Creates a new builder object for {@link GeneratorMetadata}, accepting another instance from which to copy properties.
     *
     * @param copy An existing instance to copy defaults from
     *
     * @return A new builder instance, with values preset to those of 'copy'.
     */
    public static Builder newBuilder(GeneratorMetadata copy) {
        Builder builder = new Builder();
        if (copy != null) {
            builder.stability = copy.getStability();
            builder.generationMessage = copy.getGenerationMessage();
        }
        return builder;
    }

    /**
     * Returns a message which can be displayed during generation.
     *
     * @return A message, if defined.
     */
    public String getGenerationMessage() {
        return generationMessage;
    }

    /**
     * Returns an enum describing the stability index of the generator.
     *
     * @return The defined stability index.
     */
    public Stability getStability() {
        return stability;
    }

    /**
     * {@code GeneratorMetadata} builder static inner class.
     */
    public static final class Builder {
        private Stability stability;
        private String generationMessage;

        private Builder() {
        }

        /**
         * Sets the {@code stability} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param stability the {@code stability} to set
         * @return a reference to this Builder
         */
        public Builder stability(Stability stability) {
            this.stability = stability;
            return this;
        }

        /**
         * Sets the {@code generationMessage} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param generationMessage the {@code generationMessage} to set
         * @return a reference to this Builder
         */
        public Builder generationMessage(String generationMessage) {
            this.generationMessage = generationMessage;
            return this;
        }

        /**
         * Returns a {@code GeneratorMetadata} built from the parameters previously set.
         *
         * @return a {@code GeneratorMetadata} built with parameters of this {@code GeneratorMetadata.Builder}
         */
        public GeneratorMetadata build() {
            return new GeneratorMetadata(this);
        }
    }
}
