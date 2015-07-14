package io.swagger.codegen;

import java.util.Set;

public class CodegenSecurity {
    public String name;
    public String type;
    public Boolean hasMore, isBasic, isOAuth, isApiKey;
    // ApiKey specific
    public String keyParamName;
    public Boolean isKeyInQuery, isKeyInHeader;
    // Oauth specific
    public String flow, authorizationUrl, tokenUrl;
    public Set<String> scopes;
}
