package org.openapitools.codegen;

import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

public class CodegenDiscriminator {
    private String propertyName; // propertyName: <value> converted to var name casing/
    private String propertyBaseName; // original propertyName: <value>
    private Map<String, String> mapping;
    private Set<MappedModel> mappedModels = new LinkedHashSet<>();

    public String getPropertyName() {
        return propertyName;
    }

    public void setPropertyName(String propertyName) {
        this.propertyName = propertyName;
    }

    public String getPropertyBaseName() {
        return propertyBaseName;
    }

    public void setPropertyBaseName(String propertyBaseName) {
        this.propertyBaseName = propertyBaseName;
    }

    public Map<String, String> getMapping() {
        return mapping;
    }

    public void setMapping(Map<String, String> mapping) {
        this.mapping = mapping;
    }

    public Set<MappedModel> getMappedModels() {
        return mappedModels;
    }

    public void setMappedModels(Set<MappedModel> mappedModels) {
        this.mappedModels = mappedModels;
    }

    public static class MappedModel {
        private String mappingName;
        private String modelName;

        public MappedModel(String mappingName, String modelName) {
            this.mappingName = mappingName;
            this.modelName = modelName;
        }

        public String getMappingName() {
            return mappingName;
        }

        public void setMappingName(String mappingName) {
            this.mappingName = mappingName;
        }

        public String getModelName() {
            return modelName;
        }

        public void setModelName(String modelName) {
            this.modelName = modelName;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            MappedModel that = (MappedModel) o;
            return Objects.equals(mappingName, that.mappingName) &&
                Objects.equals(modelName, that.modelName);
        }

        @Override
        public int hashCode() {
            return Objects.hash(mappingName, modelName);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenDiscriminator that = (CodegenDiscriminator) o;
        return Objects.equals(propertyName, that.propertyName) &&
            Objects.equals(propertyBaseName, that.propertyBaseName) &&
            Objects.equals(mapping, that.mapping) &&
            Objects.equals(mappedModels, that.mappedModels);
    }

    @Override
    public int hashCode() {
        return Objects.hash(propertyName, mapping, mappedModels);
    }

    @Override
    public String toString() {
        return new ToStringBuilder(this)
                .append("propertyName", propertyName)
                .append("propertyBaseName", propertyBaseName)
                .append("mapping", mapping)
                .append("mappedModels", mappedModels)
                .toString();
    }
}
