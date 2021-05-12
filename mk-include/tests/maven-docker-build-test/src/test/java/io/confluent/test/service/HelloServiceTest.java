package io.confluent.test.service;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

public class HelloServiceTest 
{
    private static HelloService helloService;

    @BeforeClass
    public static void setup()
    {
        helloService = new HelloService();
    }
    
    @AfterClass
    public static void teardown()
    {
        helloService = null;
    }

    @Test
    public void testSayHello() {
        String name = "World";
        String hello = helloService.sayHello(name);
        Assert.assertEquals("Hello World", hello);
    }
}
