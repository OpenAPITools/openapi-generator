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

        Assert.assertEquals(codegen.toApiTestFilename("Foo"), "FooSpec");
        Assert.assertEquals(codegen.toApiTestFilename("foo"), "FooSpec");
        Assert.assertEquals(codegen.toApiTestFilename("FOO"), "FOOSpec");
        Assert.assertEquals(codegen.toApiTestFilename("foo-bar"), "FooBarSpec");
        Assert.assertEquals(codegen.toApiTestFilename("foo_bar"), "FooBarSpec");
    }

    @Test
    public void testIsOverlappedPath() throws Exception {
        final HaskellYesodServerCodegen codegen = new HaskellYesodServerCodegen();
        codegen.processOpts();

        Assert.assertTrue(codegen.isOverlappedPath("/foo", "/#param"));
        Assert.assertTrue(codegen.isOverlappedPath("/#param", "/foo"));
        Assert.assertTrue(codegen.isOverlappedPath("/foo/bar", "/foo/#param"));
        Assert.assertTrue(codegen.isOverlappedPath("/foo/bar", "/#param/bar"));
        Assert.assertTrue(codegen.isOverlappedPath("/foo/bar", "/#param1/#param2"));

        Assert.assertFalse(codegen.isOverlappedPath("/foo", "/bar"));
        Assert.assertFalse(codegen.isOverlappedPath("/foo", "/foo"));
        Assert.assertFalse(codegen.isOverlappedPath("/foo", "/foo/#param"));
    }

    @Test
    public void testHasOverlappedPath() throws Exception {
        final HaskellYesodServerCodegen codegen = new HaskellYesodServerCodegen();
        codegen.processOpts();

        Assert.assertTrue(codegen.hasOverlappedPath("/foo", toRoutes("/#param")));
        Assert.assertTrue(codegen.hasOverlappedPath("/foo", toRoutes("/foo", "/#param")));
        Assert.assertTrue(codegen.hasOverlappedPath("/foo", toRoutes("/#param", "/foo")));

        Assert.assertFalse(codegen.hasOverlappedPath("/foo", toRoutes()));
        Assert.assertFalse(codegen.hasOverlappedPath("/foo", toRoutes("/bar")));
        Assert.assertFalse(codegen.hasOverlappedPath("/foo", toRoutes("!/#param")));
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
