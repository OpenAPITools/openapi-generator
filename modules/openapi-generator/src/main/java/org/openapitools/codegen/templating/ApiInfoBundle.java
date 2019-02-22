package org.openapitools.codegen.templating;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class ApiInfoBundle extends BaseBundle {

    private List<ApiBundle> apis = new ArrayList<>();

    // getters and setters. Each setter puts the value in the underlying Map

    public List<ApiBundle> getApis() {
        return this.apis;
    }

    public void setApis(List<ApiBundle> apis) {
        this.apis = apis;
        put("apis", apis);
    }
}
