package org.openapitools.codegen.v2;

import java.util.Set;

public class CodegenSdk extends CodegenObject {
    private Set<CodegenApi> apis;

    protected CodegenSdk(String id) {
        super(id);
    }

    public Set<CodegenApi> getApis() {
        return apis;
    }

    public void setApis(Set<CodegenApi> apis) {
        this.apis = apis;
    }
}
