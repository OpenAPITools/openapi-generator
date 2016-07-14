package io.swagger.codegen;

import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.auth.AuthMethod;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        ClientOpts that = (ClientOpts) o;

        if (uri != null ? !uri.equals(that.uri) : that.uri != null)
            return false;
        if (target != null ? !target.equals(that.target) : that.target != null)
            return false;
        if (auth != null ? !auth.equals(that.auth) : that.auth != null)
            return false;
        if (properties != null ? !properties.equals(that.properties) : that.properties != null)
            return false;
        return outputDirectory != null ? outputDirectory.equals(that.outputDirectory) : that.outputDirectory == null;

    }

    @Override
    public int hashCode() {
        int result = uri != null ? uri.hashCode() : 0;
        result = 31 * result + (target != null ? target.hashCode() : 0);
        result = 31 * result + (auth != null ? auth.hashCode() : 0);
        result = 31 * result + (properties != null ? properties.hashCode() : 0);
        result = 31 * result + (outputDirectory != null ? outputDirectory.hashCode() : 0);
        return result;
    }
}
