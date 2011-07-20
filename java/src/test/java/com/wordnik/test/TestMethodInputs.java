package com.wordnik.test;

import com.wordnik.api.WordAPI;
//import com.wordnik.api.WordListAPI;
import com.wordnik.exception.WordnikAPIException;
import com.wordnik.model.WordList;
import org.codehaus.jackson.JsonParser;
import org.codehaus.jackson.map.AnnotationIntrospector;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.introspect.JacksonAnnotationIntrospector;
import org.codehaus.jackson.xc.JaxbAnnotationIntrospector;
import org.junit.Test;

import java.lang.reflect.Method;

/**
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 4/15/11
 * Time: 7:02 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestMethodInputs {

    public static void main(String[] args) {

        try {
            TestMethodInputs atest = new TestMethodInputs();
            atest.testConversionToArrayOfPrimitive();
        }catch(Exception e){
            e.printStackTrace();
        }
    }

    @Test
    public void testExceptionDeserialize() throws Exception {
        String input = "{\"message\":\"No value specified for 'Date'\",\"code\":0}";
        WordnikAPIException exception = (WordnikAPIException)APITestRunner.convertJSONStringToObject(input, WordnikAPIException.class);
        assert (exception != null);
    }

    @Test
    public void testConversionToArrayOfPrimitive() throws Exception {
        String input = "[\"one\", \"two\"]";
        String[] returnType = new String[5];
        String[] inputArray = (String[])APITestRunner.convertJSONStringToObject(input, returnType.getClass());
        assert(inputArray[0].equals("one"));
    }

     @Test
    public void testConversionToArrayOfWordnikObjects() throws Exception {
        String input = "[{\"name\":\"one\", \"id\":1},{\"name\":\"two\", \"id\":2}]";
        WordList[] returnType = new WordList[5];
        WordList[] inputArray = (WordList[])APITestRunner.convertJSONStringToObject(input, returnType.getClass());
        assert(inputArray[3].getName().equals("one1"));
    }
}


