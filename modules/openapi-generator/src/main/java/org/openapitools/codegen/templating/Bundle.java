package org.openapitools.codegen.templating;

import java.util.Map;

/**
 * a Bundle is a simple {@literal Map<String, Object>}
 *     but we add strongly typed fields to avoid
 *     - get("name") | put("name", value) pseudo-properties
 *     - the back and forth casting
 */
public interface Bundle extends Map<String, Object>{
}
