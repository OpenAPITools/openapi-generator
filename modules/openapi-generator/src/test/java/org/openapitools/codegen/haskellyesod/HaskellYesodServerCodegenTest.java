package org.openapitools.codegen.haskellyesod;

import java.util.*;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.HaskellYesodServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class HaskellYesodServerCodegenTest {

    @Test
    public void testToApiTestFilename() throws Exception {
        final HaskellYesodServerCodegen codegen = new HaskellYesodServerCodegen();
        codegen.processOpts();

        Assertions.assertEquals(codegen.toApiTestFilename("Foo"), "FooSpec");
        Assertions.assertEquals(codegen.toApiTestFilename("foo"), "FooSpec");
        Assertions.assertEquals(codegen.toApiTestFilename("FOO"), "FOOSpec");
        Assertions.assertEquals(codegen.toApiTestFilename("foo-bar"), "FooBarSpec");
        Assertions.assertEquals(codegen.toApiTestFilename("foo_bar"), "FooBarSpec");
    }

    @Test
    public void testIsOverlappedPath() throws Exception {
        final HaskellYesodServerCodegen codegen = new HaskellYesodServerCodegen();
        codegen.processOpts();

        Assertions.assertTrue(codegen.isOverlappedPath("/foo", "/#param"));
        Assertions.assertTrue(codegen.isOverlappedPath("/#param", "/foo"));
        Assertions.assertTrue(codegen.isOverlappedPath("/foo/bar", "/foo/#param"));
        Assertions.assertTrue(codegen.isOverlappedPath("/foo/bar", "/#param/bar"));
        Assertions.assertTrue(codegen.isOverlappedPath("/foo/bar", "/#param1/#param2"));

        Assertions.assertFalse(codegen.isOverlappedPath("/foo", "/bar"));
        Assertions.assertFalse(codegen.isOverlappedPath("/foo", "/foo"));
        Assertions.assertFalse(codegen.isOverlappedPath("/foo", "/foo/#param"));
    }

    @Test
    public void testHasOverlappedPath() throws Exception {
        final HaskellYesodServerCodegen codegen = new HaskellYesodServerCodegen();
        codegen.processOpts();

        Assertions.assertTrue(codegen.hasOverlappedPath("/foo", toRoutes("/#param")));
        Assertions.assertTrue(codegen.hasOverlappedPath("/foo", toRoutes("/foo", "/#param")));
        Assertions.assertTrue(codegen.hasOverlappedPath("/foo", toRoutes("/#param", "/foo")));

        Assertions.assertFalse(codegen.hasOverlappedPath("/foo", toRoutes()));
        Assertions.assertFalse(codegen.hasOverlappedPath("/foo", toRoutes("/bar")));
        Assertions.assertFalse(codegen.hasOverlappedPath("/foo", toRoutes("!/#param")));
    }

    private List<Map<String, Object>> toRoutes(String... paths) {
        List<Map<String, Object>> routes = new ArrayList<Map<String, Object>>();
        for (String path : paths) {
            Map<String, Object> route = new HashMap<String, Object>();
            route.put("path", path);
            routes.add(route);
        }
        return routes;
    }
}
