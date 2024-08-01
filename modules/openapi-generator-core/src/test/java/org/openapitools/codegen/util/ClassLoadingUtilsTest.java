package org.openapitools.codegen.util;

import static org.testng.Assert.*;
import org.testng.annotations.Test;

public class ClassLoadingUtilsTest {

    /**
     * Tests existing resources both with the class source and with the class loader source. This
     * allows for multiple modes of action and for this utility to be tested under different
     * conditions where it may be used.
     *
     */
    @Test
    public void load() {

        assertNotNull(ClassLoadingUtils.getResource("class-resource-1.txt"));
        assertNotNull(ClassLoadingUtils.getResource("class-resource-1.txt", (ClassLoader)null));
        assertNotNull(ClassLoadingUtils.getResource("class-resource-1.txt", this.getClass().getClassLoader()));
        assertNotNull(ClassLoadingUtils.getResource("class-resource-1.txt", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNotNull(ClassLoadingUtils.getResource("class-resource-1.txt", ClassLoadingUtils.class.getClassLoader()));

        assertNotNull(ClassLoadingUtils.getResource("/class-resource-1.txt", this.getClass()));
        assertNull(ClassLoadingUtils.getResource("/class-resource-1.txt", (ClassLoader)null));
        assertNull(ClassLoadingUtils.getResource("/class-resource-1.txt", this.getClass().getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("/class-resource-1.txt", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("/class-resource-1.txt", ClassLoadingUtils.class.getClassLoader()));

        assertNotNull(ClassLoadingUtils.getResource("nested/class-resource-2.txt"));
        assertNotNull(ClassLoadingUtils.getResource("nested/class-resource-2.txt", (ClassLoader)null));
        assertNotNull(ClassLoadingUtils.getResource("nested/class-resource-2.txt", this.getClass().getClassLoader()));
        assertNotNull(ClassLoadingUtils.getResource("nested/class-resource-2.txt", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNotNull(ClassLoadingUtils.getResource("nested/class-resource-2.txt", ClassLoadingUtils.class.getClassLoader()));

        assertNotNull(ClassLoadingUtils.getResource("/nested/class-resource-2.txt", this.getClass()));
        assertNull(ClassLoadingUtils.getResource("/nested/class-resource-2.txt", (ClassLoader)null));
        assertNull(ClassLoadingUtils.getResource("/nested/class-resource-2.txt", this.getClass().getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("/nested/class-resource-2.txt", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("/nested/class-resource-2.txt", ClassLoadingUtils.class.getClassLoader()));

        assertNotNull(ClassLoadingUtils.getResource("nested"));
        assertNotNull(ClassLoadingUtils.getResource("nested", (ClassLoader)null));
        assertNotNull(ClassLoadingUtils.getResource("nested", this.getClass().getClassLoader()));
        assertNotNull(ClassLoadingUtils.getResource("nested", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNotNull(ClassLoadingUtils.getResource("nested", ClassLoadingUtils.class.getClassLoader()));

        assertNotNull(ClassLoadingUtils.getResource("/nested", this.getClass()));
        assertNull(ClassLoadingUtils.getResource("/nested", (ClassLoader)null));
        assertNull(ClassLoadingUtils.getResource("/nested", this.getClass().getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("/nested", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("/nested", ClassLoadingUtils.class.getClassLoader()));

    }

    @Test
    public void loadNonExisting() {

        assertNull(ClassLoadingUtils.getResource("class-resource-2.txt"));
        assertNull(ClassLoadingUtils.getResource("class-resource-2.txt", (ClassLoader)null));
        assertNull(ClassLoadingUtils.getResource("class-resource-2.txt", this.getClass().getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("class-resource-2.txt", ClassLoadingUtilsTest.class.getClassLoader()));
        assertNull(ClassLoadingUtils.getResource("class-resource-2.txt", ClassLoadingUtils.class.getClassLoader()));

    }

}
