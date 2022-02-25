package org.openapitools.codegen.kotlin;

import org.testng.Assert;
import org.testng.annotations.Test;

import java.lang.reflect.Constructor;
import java.util.Collections;

public class KotlinTestUtilsTest {

    @Test
    public void testNormalCompile() throws Exception {
        ClassLoader classLoader = KotlinTestUtils.buildModule(Collections.singletonList(getClass().getResource("KotlinTestUtilsTest/normalPack").getFile()), Thread.currentThread().getContextClassLoader());
        Class<?> clazz = classLoader.loadClass("com.example.SimpleClass");
        Constructor<?>[] constructors = clazz.getConstructors();
        Assert.assertEquals(1, constructors.length);
        Constructor<?> constr = constructors[0];
        Object testObj = constr.newInstance("test");
    }

    @Test(expectedExceptions = Exception.class)
    public void testBadCompile() {
        KotlinTestUtils.buildModule(Collections.singletonList(getClass().getResource("KotlinTestUtilsTest/badPack").getFile()), Thread.currentThread().getContextClassLoader());
    }

}
