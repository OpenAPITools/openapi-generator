package org.openapitools.codegen.model;

import java.util.HashMap;
import java.util.List;

public class ApiInfoMap extends HashMap<String, Object> {

    public void setApis(List<OperationsMap> apis) {
        put("apis", apis);
    }

    @SuppressWarnings("unchecked")
    public List<OperationsMap> getApis() {
        return (List<OperationsMap>) get("apis");
    }

}
