package io.swagger.codegen;

import io.swagger.codegen.examples.ExampleGenerator;
import io.swagger.models.Model;
import io.swagger.models.ModelImpl;
import io.swagger.models.Xml;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.RefProperty;
import io.swagger.models.properties.StringProperty;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Sets;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.Arrays;
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
        final RefProperty ref = new RefProperty(nodeType);
        final Model node = new ModelImpl().name(nodeType).property("name", new StringProperty())
                .property("parent", ref)
                .property("children", new ArrayProperty(ref))
                .property("wrappedChildren", new ArrayProperty(ref).xml(new Xml().wrapped(true)));
        final String pairType = "Pair";
        final ModelImpl pair = new ModelImpl().name(pairType);
        for (Map.Entry<String, String> item : ImmutableMap.of("first", "First", "second", "Second").entrySet()) {
            final RefProperty property = new RefProperty(nodeType);
            property.setXml(new Xml().name(item.getValue()));
            pair.property(item.getKey(), property);

        }
        final Set<String> types = Sets.newHashSet();
        final List<String> expectedTypes = Arrays.asList(JSON, XML);

        final ExampleGenerator eg = new ExampleGenerator(ImmutableMap.of(nodeType, node, pairType, pair));
        for (Map<String, String> item : eg.generate(null, expectedTypes, new RefProperty(pairType))) {
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
