package org.openapitools.codegen.v2;

import java.util.Map;
import java.util.TreeMap;

public class DefaultCodegenOptions implements CodegenOptions {

    private String project;
    private String version;
    private final Map<String, Object> additionalOptions;

    public DefaultCodegenOptions() {
        this.additionalOptions = new TreeMap<>();
    }

    @Override
    public String getProject() {
        return project;
    }

    @Override
    public String getVersion() {
        return version;
    }

    @Override
    public Map<String, Object> getAdditionalOptions() {
        return additionalOptions;
    }

    public void setProject(String project) {
        this.project = project;
    }

    public void setVersion(String version) {
        this.version = version;
    }
}
