package org.openapitools.codegen;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class CodegenDiscriminator {
    private String propertyName;
    private Map<String, String> mapping;
    private List<MappedModel> mappedModels = new ArrayList<>();

    public String getPropertyName() {
        return propertyName;
    }

    public void setPropertyName(String propertyName) {
        this.propertyName = propertyName;
    }

    public Map<String, String> getMapping() {
        return mapping;
    }

    public void setMapping(Map<String, String> mapping) {
        this.mapping = mapping;
    }

    public List<MappedModel> getMappedModels() {
        return mappedModels;
    }

    public void setMappedModels(List<MappedModel> mappedModels) {
        this.mappedModels = mappedModels;
    }

    public static class MappedModel {
        private String mappingName;
        private CodegenModel model;

        public String getMappingName() {
            return mappingName;
        }

        public void setMappingName(String mappingName) {
            this.mappingName = mappingName;
        }

        public CodegenModel getModel() {
            return model;
        }

        public void setModel(CodegenModel model) {
            this.model = model;
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenDiscriminator that = (CodegenDiscriminator) o;
        return Objects.equals(propertyName, that.propertyName) &&
            Objects.equals(mapping, that.mapping);
    }

    @Override
    public int hashCode() {
        return Objects.hash(propertyName, mapping);
    }
}
