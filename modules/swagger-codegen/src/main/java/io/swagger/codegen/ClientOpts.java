package io.swagger.codegen;

import java.util.HashMap;
import java.util.Map;

import io.swagger.codegen.auth.AuthMethod;

public class ClientOpts {
    protected AuthMethod auth;
    protected Map<String, String> properties = new HashMap<String, String>();

    public Map<String, String> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("ClientOpts: {\n");
        sb.append("  auth: ").append(auth).append(",");
        sb.append(properties);
        sb.append("}");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }

        ClientOpts that = (ClientOpts) o;

        if (auth != null ? !auth.equals(that.auth) : that.auth != null) { return false; }
        return getProperties().equals(that.getProperties());

    }

    @Override
    public int hashCode() {
        int result = auth != null ? auth.hashCode() : 0;
        result = 31 * result + getProperties().hashCode();
        return result;
    }
}
