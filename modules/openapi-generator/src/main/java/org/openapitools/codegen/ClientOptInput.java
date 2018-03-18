package org.openapitools.codegen;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import java.util.List;
import org.openapitools.codegen.auth.AuthParser;

public class ClientOptInput {
    private CodegenConfig config;
    private ClientOpts opts;
    private OpenAPI openAPI;
    private List<AuthorizationValue> auths;

    public ClientOptInput openAPI(OpenAPI openAPI) {
        this.setOpenAPI(openAPI);
        return this;
    }

    public ClientOptInput opts(ClientOpts opts) {
        this.setOpts(opts);
        return this;
    }

    public ClientOptInput config(CodegenConfig codegenConfig) {
        this.setConfig(codegenConfig);
        return this;
    }

    @Deprecated
    public ClientOptInput auth(String urlEncodedAuthString) {
        this.setAuth(urlEncodedAuthString);
        return this;
    }

    @Deprecated
    public String getAuth() {
        return AuthParser.reconstruct(auths);
    }

    @Deprecated
    public void setAuth(String urlEncodedAuthString) {
        this.auths = AuthParser.parse(urlEncodedAuthString);
    }

    @Deprecated
    public List<AuthorizationValue> getAuthorizationValues() {
        return auths;
    }

    public CodegenConfig getConfig() {
        return config;
    }

    public void setConfig(CodegenConfig config) {
        this.config = config;
    }

    public ClientOpts getOpts() {
        return opts;
    }

    public void setOpts(ClientOpts opts) {
        this.opts = opts;
    }

    public OpenAPI getOpenAPI() {
        return openAPI;
    }

    public void setOpenAPI(OpenAPI openAPI) {
        this.openAPI = openAPI;
    }
}
