package io.swagger.codegen;

import java.util.List;
import java.util.Map;

public class CodegenSecurity {
    public String name;
    public String type;
    public Boolean hasMore, isBasic, isOAuth, isApiKey;
    public Map<String, Object> vendorExtensions;
    // ApiKey specific
    public String keyParamName;
    public Boolean isKeyInQuery, isKeyInHeader;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public List<Map<String, Object>> scopes;
    public Boolean isCode, isPassword, isApplication, isImplicit;

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
        if (hasMore != null ? !hasMore.equals(that.hasMore) : that.hasMore != null)
            return false;
        if (isBasic != null ? !isBasic.equals(that.isBasic) : that.isBasic != null)
            return false;
        if (isOAuth != null ? !isOAuth.equals(that.isOAuth) : that.isOAuth != null)
            return false;
        if (isApiKey != null ? !isApiKey.equals(that.isApiKey) : that.isApiKey != null)
            return false;
        if (vendorExtensions != null ? !vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions != null)
            return false;
        if (keyParamName != null ? !keyParamName.equals(that.keyParamName) : that.keyParamName != null)
            return false;
        if (isKeyInQuery != null ? !isKeyInQuery.equals(that.isKeyInQuery) : that.isKeyInQuery != null)
            return false;
        if (isKeyInHeader != null ? !isKeyInHeader.equals(that.isKeyInHeader) : that.isKeyInHeader != null)
            return false;
        if (flow != null ? !flow.equals(that.flow) : that.flow != null)
            return false;
        if (authorizationUrl != null ? !authorizationUrl.equals(that.authorizationUrl) : that.authorizationUrl != null)
            return false;
        if (tokenUrl != null ? !tokenUrl.equals(that.tokenUrl) : that.tokenUrl != null)
            return false;
        if (isCode != null ? !isCode.equals(that.isCode) : that.isCode != null)
            return false;
        if (isPassword != null ? !isPassword.equals(that.isPassword) : that.isPassword != null)
            return false;
        if (isApplication != null ? !isApplication.equals(that.isApplication) : that.isApplication != null)
            return false;
        if (isImplicit != null ? !isImplicit.equals(that.isImplicit) : that.isImplicit != null)
            return false;
        return scopes != null ? scopes.equals(that.scopes) : that.scopes == null;

    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (type != null ? type.hashCode() : 0);
        result = 31 * result + (hasMore != null ? hasMore.hashCode() : 0);
        result = 31 * result + (isBasic != null ? isBasic.hashCode() : 0);
        result = 31 * result + (isOAuth != null ? isOAuth.hashCode() : 0);
        result = 31 * result + (isApiKey != null ? isApiKey.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + (keyParamName != null ? keyParamName.hashCode() : 0);
        result = 31 * result + (isKeyInQuery != null ? isKeyInQuery.hashCode() : 0);
        result = 31 * result + (isKeyInHeader != null ? isKeyInHeader.hashCode() : 0);
        result = 31 * result + (flow != null ? flow.hashCode() : 0);
        result = 31 * result + (authorizationUrl != null ? authorizationUrl.hashCode() : 0);
        result = 31 * result + (tokenUrl != null ? tokenUrl.hashCode() : 0);
        result = 31 * result + (isCode != null ? isCode.hashCode() : 0);
        result = 31 * result + (isPassword != null ? isPassword.hashCode() : 0);
        result = 31 * result + (isApplication != null ? isApplication.hashCode() : 0);
        result = 31 * result + (isImplicit != null ? isImplicit.hashCode() : 0);
        result = 31 * result + (scopes != null ? scopes.hashCode() : 0);
        return result;
    }
}
