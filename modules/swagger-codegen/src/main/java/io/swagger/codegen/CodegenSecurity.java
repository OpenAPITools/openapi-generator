package io.swagger.codegen;

import io.swagger.oas.models.security.Scopes;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CodegenSecurity implements VendorExtendable {
    public String name;
    public String type;
    // ApiKey specific
    public String keyParamName;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public Scopes scopes;
    public Map<String, Object> vendorExtensions = new HashMap<>();

    @Override
    public String toString() {
        return String.format("%s(%s)", name, type);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenSecurity that = (CodegenSecurity) o;

        if (name != null ? !name.equals(that.name) : that.name != null)
            return false;
        if (type != null ? !type.equals(that.type) : that.type != null)
            return false;
        if (keyParamName != null ? !keyParamName.equals(that.keyParamName) : that.keyParamName != null)
            return false;
        if (flow != null ? !flow.equals(that.flow) : that.flow != null)
            return false;
        if (authorizationUrl != null ? !authorizationUrl.equals(that.authorizationUrl) : that.authorizationUrl != null)
            return false;
        if (tokenUrl != null ? !tokenUrl.equals(that.tokenUrl) : that.tokenUrl != null)
            return false;
        return scopes != null ? scopes.equals(that.scopes) : that.scopes == null;

    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (keyParamName != null ? keyParamName.hashCode() : 0);
        result = 31 * result + (flow != null ? flow.hashCode() : 0);
        result = 31 * result + (authorizationUrl != null ? authorizationUrl.hashCode() : 0);
        result = 31 * result + (tokenUrl != null ? tokenUrl.hashCode() : 0);
        result = 31 * result + (scopes != null ? scopes.hashCode() : 0);
        return result;
    }

    public String getName() {
        return name;
    }

    public String getType() {
        return type;
    }

    public String getKeyParamName() {
        return keyParamName;
    }

    public String getFlow() {
        return flow;
    }

    public String getAuthorizationUrl() {
        return authorizationUrl;
    }

    public String getTokenUrl() {
        return tokenUrl;
    }

    public Scopes getScopes() {
        return scopes;
    }

    @Override
    public Map<String, Object> getVendorExtensions() {
        return this.vendorExtensions;
    }
}
