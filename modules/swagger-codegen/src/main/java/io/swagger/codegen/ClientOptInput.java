package io.swagger.codegen;

import io.swagger.annotations.ApiModelProperty;
import io.swagger.models.Swagger;
import io.swagger.models.auth.AuthorizationValue;

import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

public class ClientOptInput {
    private CodegenConfig config;
    private ClientOpts opts;
    private Swagger swagger;
    private List<AuthorizationValue> auths;

    public ClientOptInput swagger(Swagger swagger) {
        this.setSwagger(swagger);
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

    public String getAuth() {
        if (auths != null) {
            StringBuilder b = new StringBuilder();
            for (AuthorizationValue v : auths) {
                try {
                    if (b.toString().length() > 0) {
                        b.append(",");
                    }
                    b.append(URLEncoder.encode(v.getKeyName(), "UTF-8"))
                            .append(":")
                            .append(URLEncoder.encode(v.getValue(), "UTF-8"));
                } catch (Exception e) {
                    // continue
                    e.printStackTrace();
                }
            }
            return b.toString();
        } else {
            return null;
        }
    }

    public void setAuth(String urlEncodedAuthString) {
        List<AuthorizationValue> auths = new ArrayList<AuthorizationValue>();
        if (urlEncodedAuthString != null && !"".equals(urlEncodedAuthString)) {
            String[] parts = urlEncodedAuthString.split(",");
            for (String part : parts) {
                String[] kvPair = part.split(":");
                if (kvPair.length == 2) {
                    auths.add(new AuthorizationValue(URLDecoder.decode(kvPair[0]), URLDecoder.decode(kvPair[1]), "header"));
                }
            }
        }
        this.auths = auths;
    }

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

    @ApiModelProperty(dataType = "Object")
    public Swagger getSwagger() {
        return swagger;
    }

    public void setSwagger(Swagger swagger) {
        this.swagger = swagger;
    }
}