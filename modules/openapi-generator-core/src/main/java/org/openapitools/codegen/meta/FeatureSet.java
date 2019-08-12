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

import org.openapitools.codegen.meta.features.*;

import java.util.EnumSet;

/**
 * Defines the feature set for a target generator.
 */
@SuppressWarnings({"unused", "WeakerAccess"})
public class FeatureSet {
    public static FeatureSet UNSPECIFIED = FeatureSet.newBuilder().build();

    private EnumSet<ClientModificationFeature> clientModificationFeatures;
    private EnumSet<DataTypeFeature> dataTypeFeatures;
    private EnumSet<DocumentationFeature> documentationFeature;
    private EnumSet<ModelReuseFeature> modelReuseFeature;
    private EnumSet<ParameterFeature> parameterFeature;
    private EnumSet<SecurityFeature> securityFeature;

    private FeatureSet(Builder builder) {
        if (builder != null) {
            clientModificationFeatures = builder.clientModificationFeatures;
            dataTypeFeatures = builder.dataTypeFeatures;
            documentationFeature = builder.documentationFeature;
            modelReuseFeature = builder.modelReuseFeature;
            parameterFeature = builder.parameterFeature;
            securityFeature = builder.securityFeature;
        }
    }

    public static Builder newBuilder() {
        return new Builder();
    }

    public static Builder newBuilder(FeatureSet copy) {
        Builder builder = new Builder();
        if (copy != null) {
            builder.clientModificationFeatures = copy.getClientModificationFeatures();
            builder.dataTypeFeatures = copy.getDataTypeFeatures();
            builder.documentationFeature = copy.getDocumentationFeature();
            builder.modelReuseFeature = copy.getModelReuseFeature();
            builder.parameterFeature = copy.getParameterFeature();
            builder.securityFeature = copy.getSecurityFeature();
        }
        return builder;
    }

    /**
     * Returns the set of client modification features supported by the generator.
     *
     * @return A new copy of the defined feature set. Changes to this instance are not promoted.
     */
    public EnumSet<ClientModificationFeature> getClientModificationFeatures() {
        if (clientModificationFeatures != null) {
            return EnumSet.copyOf(clientModificationFeatures);
        } else {
            return EnumSet.noneOf(ClientModificationFeature.class);
        }
    }

    /**
     * Returns the set of common data types supported by the generator
     *
     * @return A new copy of the defined feature set. Changes to this instance are not promoted.
     */
    public EnumSet<DataTypeFeature> getDataTypeFeatures() {
        if (dataTypeFeatures != null) {
            return EnumSet.copyOf(dataTypeFeatures);
        } else {
            return EnumSet.noneOf(DataTypeFeature.class);
        }
    }

    /**
     * Returns the documentation type available in generated output.
     *
     * @return A new copy of the defined feature set. Changes to this instance are not promoted.
     */
    public EnumSet<DocumentationFeature> getDocumentationFeature() {
        if (documentationFeature != null) {
            return EnumSet.copyOf(documentationFeature);
        } else {
            return EnumSet.noneOf(DocumentationFeature.class);
        }
    }

    /**
     * Returns special circumstances handled by the generator.
     *
     * @return A new copy of the defined feature set. Changes to this instance are not promoted.
     */
    public EnumSet<ModelReuseFeature> getModelReuseFeature() {
        if (modelReuseFeature != null) {
            return EnumSet.copyOf(modelReuseFeature);
        } else {
            return EnumSet.noneOf(ModelReuseFeature.class);
        }
    }

    /**
     * Returns the types of parameters supported by endpoints in the generated code.
     *
     * @return A new copy of the defined feature set. Changes to this instance are not promoted.
     */
    public EnumSet<ParameterFeature> getParameterFeature() {
        if (parameterFeature != null) {
            return EnumSet.copyOf(parameterFeature);
        } else {
            return EnumSet.noneOf(ParameterFeature.class);
        }
    }

    /**
     * Returns the security features supported in the generated code.
     *
     * @return A new copy of the defined feature set. Changes to this instance are not promoted.
     */
    public EnumSet<SecurityFeature> getSecurityFeature() {
        if (securityFeature != null) {
            return EnumSet.copyOf(securityFeature);
        } else {
            return EnumSet.noneOf(SecurityFeature.class);
        }
    }

    /**
     * {@code FeatureSet} builder static inner class.
     */
    public static final class Builder {
        private EnumSet<ClientModificationFeature> clientModificationFeatures;
        private EnumSet<DataTypeFeature> dataTypeFeatures;
        private EnumSet<DocumentationFeature> documentationFeature;
        private EnumSet<ModelReuseFeature> modelReuseFeature;
        private EnumSet<ParameterFeature> parameterFeature;
        private EnumSet<SecurityFeature> securityFeature;

        private Builder() {
        }

        /**
         * Sets the {@code clientModificationFeatures} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param clientModificationFeatures the {@code clientModificationFeatures} to set
         * @return a reference to this Builder
         */
        public Builder clientModificationFeatures(EnumSet<ClientModificationFeature> clientModificationFeatures) {
            if (clientModificationFeatures != null) {
                this.clientModificationFeatures = clientModificationFeatures;
            } else {
                this.clientModificationFeatures = EnumSet.noneOf(ClientModificationFeature.class);
            }
            return this;
        }

        /**
         * Sets the {@code dataTypeFeatures} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param dataTypeFeatures the {@code dataTypeFeatures} to set
         * @return a reference to this Builder
         */
        public Builder dataTypeFeatures(EnumSet<DataTypeFeature> dataTypeFeatures) {
            if (dataTypeFeatures != null) {
                this.dataTypeFeatures = dataTypeFeatures;
            } else {
                this.dataTypeFeatures = EnumSet.noneOf(DataTypeFeature.class);
            }
            return this;
        }

        /**
         * Sets the {@code documentationFeature} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param documentationFeature the {@code documentationFeature} to set
         * @return a reference to this Builder
         */
        public Builder documentationFeature(EnumSet<DocumentationFeature> documentationFeature) {
            if (documentationFeature != null) {
                this.documentationFeature = documentationFeature;
            } else {
                this.documentationFeature = EnumSet.noneOf(DocumentationFeature.class);
            }
            return this;
        }

        /**
         * Sets the {@code modelReuseFeature} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param modelReuseFeature the {@code modelReuseFeature} to set
         * @return a reference to this Builder
         */
        public Builder modelReuseFeature(EnumSet<ModelReuseFeature> modelReuseFeature) {
            if (modelReuseFeature != null) {
                this.modelReuseFeature = modelReuseFeature;
            } else {
                this.modelReuseFeature = EnumSet.noneOf(ModelReuseFeature.class);
            }
            return this;
        }

        /**
         * Sets the {@code parameterFeature} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param parameterFeature the {@code parameterFeature} to set
         * @return a reference to this Builder
         */
        public Builder parameterFeature(EnumSet<ParameterFeature> parameterFeature) {
            if (parameterFeature != null) {
                this.parameterFeature = parameterFeature;
            } else {
                this.parameterFeature = EnumSet.noneOf(ParameterFeature.class);
            }
            return this;
        }

        /**
         * Sets the {@code securityFeature} and returns a reference to this Builder so that the methods can be chained together.
         *
         * @param securityFeature the {@code securityFeature} to set
         * @return a reference to this Builder
         */
        public Builder securityFeature(EnumSet<SecurityFeature> securityFeature) {
            if (securityFeature != null) {
                this.securityFeature = securityFeature;
            } else {
                this.securityFeature = EnumSet.noneOf(SecurityFeature.class);
            }
            return this;
        }


        /**
         * Returns a {@code FeatureSet} built from the parameters previously set.
         *
         * @return a {@code FeatureSet} built with parameters of this {@code FeatureSet.Builder}
         */
        public FeatureSet build() {
            return new FeatureSet(this);
        }
    }
}
