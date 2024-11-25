package org.openapitools.codegen.model;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class WebhooksMap extends HashMap<String, Object> {
    public OperationMap getWebhooks() {
        return (OperationMap) get("operations");
    }

    public void setWebhooks(OperationMap objs) {
        put("operations", objs);
    }

    @SuppressWarnings("unchecked")
    public List<Map<String, String>> getImports() {
        return (List<Map<String, String>>) get("imports");
    }

    public void setImports(List<Map<String, String>> imports) {
        put("imports", imports);
    }
}