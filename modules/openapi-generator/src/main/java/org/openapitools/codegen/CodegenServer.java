package org.openapitools.codegen;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class CodegenServer {
    public String url;
    public String description;
    public List<CodegenServerVariable> variables;
    public Map<String, Object> vendorExtensions = new HashMap<>();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenServer that = (CodegenServer) o;
        return Objects.equals(url, that.url) &&
                Objects.equals(description, that.description) &&
                Objects.equals(variables, that.variables) &&
                Objects.equals(vendorExtensions, that.vendorExtensions);
    }

    @Override
    public int hashCode() {

        return Objects.hash(url, description, variables, vendorExtensions);
    }

    @Override
    public String toString() {
        final StringBuffer sb = new StringBuffer("CodegenServer{");
        sb.append("url='").append(url).append('\'');
        sb.append(", description='").append(description).append('\'');
        sb.append(", variables=").append(variables);
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append('}');
        return sb.toString();
    }
}
