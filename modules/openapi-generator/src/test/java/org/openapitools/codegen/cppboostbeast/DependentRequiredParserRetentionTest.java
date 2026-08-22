package org.openapitools.codegen.cppboostbeast;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.junit.jupiter.api.Test;

import java.nio.file.Paths;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Wave-4 regression pin: swagger-parser CORRUPTS multi-entry
 * dependentRequired maps on the OAS 3.1 path — every entry's required list
 * is the MERGED union of all entries' lists (observed on two-entry maps with
 * exotic keys: the on-disk spec has ["foo\rbar"] and ["foo'bar"], the parsed
 * JsonSchema yields [foo\rbar, foo'bar] for BOTH triggers). The generator
 * does NOT trust the native getter: recoverPristineLiterals (c) injects the
 * exact literal map via x-oas31-dependent-required, and the IR readers
 * prefer that extension. If a parser upgrade FIXES this merge, this test
 * fails and the generator can switch back to getDependentRequired().
 */
public class DependentRequiredParserRetentionTest {

    private static final String SPEC = "src/test/resources/3_1/cpp-boost-beast-client/fixtures/spec_dependentRequired_3.json";

    @Test
    public void parserMergesDependentRequiredListsOnOas31Path() throws Exception {
        String path = Paths.get(SPEC).toAbsolutePath().toString();
        ParseOptions po = new ParseOptions();
        OpenAPI api = new OpenAPIV3Parser().readLocation(path, null, po)
                .getOpenAPI();
        Schema g0 = api.getComponents().getSchemas().get("G0");
        assertNotNull(g0, "G0 component must parse");
        Schema member = (Schema) g0.getOneOf().get(0);
        assertNotNull(member, "G0 must have a oneOf member");
        Map<String, List<String>> m = member.getDependentRequired();
        assertNotNull(m, "dependentRequired must be read");
        assertEquals(2, m.size(), "both triggers must survive");
        // Exotic keys survive; the REQUIRED LISTS are the corruption (merged).
        // 1) first trigger's list claims the second trigger's member too;
        // 2) both triggers share the SAME list object.
        List<String> first = null;
        boolean seenCr = false;
        boolean seenQuote = false;
        for (Map.Entry<String, List<String>> e : m.entrySet()) {
            if (e.getKey().indexOf('\n') >= 0) {
                first = e.getValue();
            }
            for (String r : e.getValue()) {
                if (r.indexOf('\r') >= 0) seenCr = true;
                if (r.indexOf('\'') >= 0) seenQuote = true;
            }
        }
        assertNotNull(first, "CRLF trigger must be present");
        assertTrue(seenCr && seenQuote,
                "parsed lists are the MERGED union (cr+quote member both "
                + "listed); if this fails the parser was fixed — switch the "
                + "generator back to getDependentRequired()");
    }
}