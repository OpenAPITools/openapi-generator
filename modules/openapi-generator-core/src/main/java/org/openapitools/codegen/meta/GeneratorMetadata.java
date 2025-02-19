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

package org.openapitools.codegen.meta;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents metadata about a generator.
 */
@SuppressWarnings("WeakerAccess")
public class GeneratorMetadata {
    private Stability stability;
    private Map<String, FeatureSet> libraryFeatures;
    private FeatureSet featureSet;
    private String generationMessage;

    private GeneratorMetadata(Builder builder) {
        if (builder != null) {
            stability = builder.stability;
            generationMessage = builder.generationMessage;
            libraryFeatures = builder.libraryFeatures;
            featureSet = builder.featureSet;
        }
    }

    /**
     * Creates a new builder object for {@link GeneratorMetadata}.
     *
     * @return A new builder instance.
     */
    public static Builder newBuilder() {
        return new Builder();
    }

    public static Builder newBuilder(GeneratorMetadata copy) {
        Builder builder = new Builder();
        if (copy != null) {
            builder.stability = copy.getStability();
            builder.generationMessage = copy.getGenerationMessage();
            builder.libraryFeatures = copy.getLibraryFeatures();
            builder.featureSet = copy.getFeatureSet();
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
     * Returns the feature set supported by the generator.
     *
     * @return The set of available features.
     */
    public FeatureSet getFeatureSet() {
        return featureSet;
    }

    /**
     * Returns the list of features supported by generator libraries.
     *
     * @return A map of library name to feature set for that library.
     */
    public Map<String, FeatureSet> getLibraryFeatures() {
        return libraryFeatures;
    }

    /**
     * {@code GeneratorMetadata} builder static inner class.
     */
    public static final class Builder {
        private Stability stability;
        private String generationMessage;
        private FeatureSet featureSet = FeatureSet.UNSPECIFIED;
        private Map<String, FeatureSet> libraryFeatures = new HashMap<>();

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
         * Sets the {@code featureSet} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param featureSet the {@code featureSet} to set
         * @return a reference to this Builder
         */
        public Builder featureSet(FeatureSet featureSet) {
            if (featureSet != null) {
                this.featureSet = featureSet;
            } else {
                this.featureSet = FeatureSet.UNSPECIFIED;
            }
            return this;
        }

        /**
         * Sets the {@code libraryFeatures} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param libraryFeatures the {@code libraryFeatures} to set
         * @return a reference to this Builder
         */
        public Builder libraryFeatures(Map<String, FeatureSet> libraryFeatures) {
            this.libraryFeatures = libraryFeatures;
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
