package org.openapitools.codegen.templating;

import java.util.HashMap;
import java.util.Map;

/**
 * simply extend HashMap
 */
class BaseBundle extends HashMap<String, Object> implements Bundle {

    BaseBundle() {
    }

    public BaseBundle(Map<String, Object> objs) {
        this.putAll(objs);
    }
}
