package com.wordnik.test;

import com.wordnik.model.WordList;
import org.junit.Test;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 4/21/11
 * Time: 8:35 AM
 * To change this template use File | Settings | File Templates.
 */
public class TestAPITestRunner {

    public List<String> primitiveStringListType = null;
    public List<WordList> objectStringListType = null;

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
        assert(inputArray[0].getName().equals("one1"));
    }

    @Test
    public void testConversionToListOfPrimitive() throws Exception {
        String input = "[\"one\", \"two\"]";
        Class argClass = null;
        for(Method method :this.getClass().getMethods()){
            if(method.getName().equals("methodWithArrayObjectInput")){
                argClass = method.getParameterTypes()[0];
            }
        }
        List<String> inputArray = (List<String>)APITestRunner.convertJSONStringToObject(input, argClass);
        assert(inputArray.get(0).equals("one"));
    }

    @Test
    public void testConversionToListOfWordnikObjects() throws Exception {
        String input = "[{\"name\":\"one\", \"id\":1},{\"name\":\"two\", \"id\":2}]";
        Class argClass = null;
        for(Method method :this.getClass().getMethods()){
            if(method.getName().equals("methodWithListObjectInput")){
                argClass = method.getParameterTypes()[0];
            }
        }
        List<WordList> inputArray = (List<WordList>)APITestRunner.convertJSONStringToObject(input, argClass);
        assert(inputArray.get(0).getName().equals("one"));
    }

    /**
     * Tets method to read argument types
     */
    public void methodWithListObjectInput(List<WordList> list){

    }

    /**
     * Tets method to read argument types
     */
    public void methodWithArrayObjectInput(WordList[] list){

    }
}
