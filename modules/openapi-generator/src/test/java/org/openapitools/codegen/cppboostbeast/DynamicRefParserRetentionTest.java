package org.openapitools.codegen.cppboostbeast;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.TestUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * Wave-3 $dynamicRef slice, advisor step 1: prove what swagger-parser retains
 * for $dynamicRef / $dynamicAnchor / $anchor / embedded $id and x-oas31
 * extension markers BEFORE designing the emitter/engine on top of it.
 */
public class DynamicRefParserRetentionTest {

    @Test
    public void parserRetainsDynamicRefConstructs() throws Exception {
        String spec = "{\n"
            + "  \"openapi\": \"3.1.0\",\n"
            + "  \"info\": {\"title\": \"dyn\", \"version\": \"1\"},\n"
            + "  \"paths\": {},\n"
            + "  \"components\": {\"schemas\": {\n"
            + "    \"S\": {\n"
            + "      \"type\": \"array\",\n"
            + "      \"$dynamicAnchor\": \"items\",\n"
            + "      \"$anchor\": \"plain\",\n"
            + "      \"$id\": \"urn:embedded\",\n"
            + "      \"items\": {\"$dynamicRef\": \"#items\"},\n"
            + "      \"x-oas31-resource\": 7\n"
            + "    }\n"
            + "  }}\n"
            + "}";
        io.swagger.parser.OpenAPIParser parser = new io.swagger.parser.OpenAPIParser();
        io.swagger.v3.parser.core.models.ParseOptions opts =
                new io.swagger.v3.parser.core.models.ParseOptions();
        opts.setResolveFully(false);
        OpenAPI api = parser.readContents(spec, null, opts).getOpenAPI();
        Assert.assertNotNull(api, "spec must parse");
        Schema<?> s = api.getComponents().getSchemas().get("S");
        Assert.assertNotNull(s, "S must parse");
        Schema<?> items = s.getItems();
        Assert.assertNotNull(items, "items must parse");
        Assert.assertEquals(items.get$dynamicRef(), "#items",
                "$dynamicRef must be retained verbatim, not resolved/dropped");
        Assert.assertEquals(s.get$dynamicAnchor(), "items",
                "$dynamicAnchor must be retained for dynamic-scope registration");
        Assert.assertEquals(s.get$anchor(), "plain",
                "$anchor must be retained verbatim");
        Assert.assertEquals(s.get$id(), "urn:embedded",
                "embedded $id must be retained verbatim");
        Object resExt = s.getExtensions() == null ? null : s.getExtensions().get("x-oas31-resource");
        Assert.assertEquals(String.valueOf(resExt), "7",
                "x-oas31 extension marker must survive parsing");
    }

    @Test
    public void parserRetainsNestedItemsDynamicRefMarkers() throws Exception {
        // Corpus critical: the runner rewrites $dynamicRef to $ref +
        // x-oas31-dynref on NESTED items schemas (e.g. dynamicRef g13
        // tree.json children.items). The emitter reads the marker from
        // schema.getExtensions() on that nested Schema; if swagger-parser
        // drops extensions on nested items, the $dynamicRef degrades to a
        // plain static $ref and dynamic-scope replacement silently never
        // happens (the strict-tree/Wave-4 FAIL family).
        String spec = "{\n"
            + "  \"openapi\": \"3.1.0\",\n"
            + "  \"info\": {\"title\": \"dyn-nested\", \"version\": \"1\"},\n"
            + "  \"paths\": {},\n"
            + "  \"components\": {\"schemas\": {\n"
            + "    \"S\": {\n"
            + "      \"type\": \"object\",\n"
            + "      \"properties\": {\n"
            + "        \"children\": {\n"
            + "          \"type\": \"array\",\n"
            + "          \"items\": {\n"
            + "            \"$ref\": \"#/components/schemas/T\",\n"
            + "            \"x-oas31-dynref\": \"node\"\n"
            + "          }\n"
            + "        }\n"
            + "      }\n"
            + "    },\n"
            + "    \"T\": {\"type\": \"string\"}\n"
            + "  }}\n"
            + "}";
        io.swagger.parser.OpenAPIParser parser = new io.swagger.parser.OpenAPIParser();
        io.swagger.v3.parser.core.models.ParseOptions opts =
                new io.swagger.v3.parser.core.models.ParseOptions();
        opts.setResolveFully(false);
        OpenAPI api = parser.readContents(spec, null, opts).getOpenAPI();
        Schema<?> items = ((Schema<?>) api.getComponents().getSchemas().get("S")
                .getProperties().get("children")).getItems();
        Assert.assertNotNull(items, "items must parse");
        Object marker = items.getExtensions() == null ? null
                : items.getExtensions().get("x-oas31-dynref");
        Assert.assertEquals(String.valueOf(marker), "node",
                "x-oas31-dynref must survive parsing on NESTED items");
    }
}
