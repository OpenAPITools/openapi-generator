package io.swagger.codegen;

import io.swagger.codegen.auth.AuthMethod;

import java.util.HashMap;
import java.util.Map;

public class ClientOpts {
    protected String uri;
    protected String target;
    protected AuthMethod auth;
    protected Map<String, String> properties = new HashMap<String, String>();
    protected String outputDirectory;

    public String getUri() {
        return uri;
    }

    public void setUri(String uri) {
        this.uri = uri;
    }

    public String getTarget() {
        return target;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public Map<String, String> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    public String getOutputDirectory() {
        return outputDirectory;
    }

    public void setOutputDirectory(String outputDirectory) {
        this.outputDirectory = outputDirectory;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ClientOpts: {\n");
        sb.append("  uri: ").append(uri).append(",");
        sb.append("  auth: ").append(auth).append(",");
        sb.append(properties);
        sb.append("}");
        return sb.toString();
    }
}
