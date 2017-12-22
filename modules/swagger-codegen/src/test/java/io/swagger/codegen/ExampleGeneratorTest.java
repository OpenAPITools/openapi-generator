package io.swagger.codegen;

import io.swagger.codegen.examples.ExampleGenerator;
/**
 import io.swagger.models.Model;
 import io.swagger.models.ModelImpl;
 import io.swagger.models.Xml;
 import io.swagger.models.properties.ArrayProperty;
 import io.swagger.models.properties.RefProperty;
 import io.swagger.models.properties.StringProperty;
 */

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import io.swagger.oas.models.media.ArraySchema;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import io.swagger.oas.models.media.XML;
import io.swagger.util.RefUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

@SuppressWarnings("static-method")
public class ExampleGeneratorTest {

    @Test(description = "check handling of recursive models")
    public void recursiveModelsTest() {
        final String JSON = "application/json";
        final String XML = "application/xml";
        final String nodeType = "Node";
        final String pairType = "Pair";
        final Schema refSchema = new Schema()
                .$ref(nodeType)
                .type("ref");
        final Schema nodeSchema = new Schema()
                .name(nodeType)
                .addProperties("parent", refSchema)
                .addProperties("name", new StringSchema())
                .addProperties("children", new ArraySchema().items(refSchema))
                .addProperties("wrappedChildren", new ArraySchema()
                        .items(refSchema)
                        .xml(new XML().wrapped(true)));
        final Schema pairSchema = new Schema()
                .name(pairType)
                .properties(new LinkedHashMap<String, Schema>());

        for (Map.Entry<String, String> item : ImmutableMap.of("first", "First", "second", "Second").entrySet()) {
            final Schema property = new Schema().type("ref").$ref(nodeType);
            property.setXml(new XML().name(item.getValue()));
            pairSchema.getProperties().put(item.getKey(), property);
        }

        final Set<String> types = Sets.newHashSet();
        final List<String> expectedTypes = Arrays.asList(JSON, XML);

        final ExampleGenerator eg = new ExampleGenerator(
                ImmutableMap.of(RefUtils.constructRef(nodeType), nodeSchema, RefUtils.constructRef(pairType), pairSchema));

        for (Map<String, String> item : eg.generate(null, expectedTypes, new Schema().type("ref").$ref(pairType))) {
            final String example = item.get("example");
            final String contentType = item.get("contentType");
            if (XML.equals(contentType)) {
                types.add(XML);
                Assert.assertEquals(example, "<Pair>\n" +
                        "  <Node>\n" +
                        "    <name>aeiou</name>\n" +
                        "    <wrappedChildren>\n" +
                        "    </wrappedChildren>\n" +
                        "  </Node>\n" +
                        "  <Node>\n" +
                        "    <name>aeiou</name>\n" +
                        "    <wrappedChildren>\n" +
                        "    </wrappedChildren>\n" +
                        "  </Node>\n" +
                        "</Pair>");
            } else if (JSON.equals(contentType)) {
                types.add(JSON);
                // TODO - add JSON validation
                Assert.assertNotNull(example);
            }
        }

        Assert.assertEqualsNoOrder(types.toArray(new String[types.size()]),
                expectedTypes.toArray(new String[expectedTypes.size()]));
    }
}
