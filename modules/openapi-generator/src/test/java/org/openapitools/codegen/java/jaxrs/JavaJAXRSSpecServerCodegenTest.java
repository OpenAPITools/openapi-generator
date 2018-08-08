package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.Operation;
import org.junit.Test;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertThat;

/**
 * Unit-Test for {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen}.
 *
 * @author attrobit
 */
public class JavaJAXRSSpecServerCodegenTest {
    
    /**
     * Test
     * {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path "/" and set tag.
     */
    @Test
    public void testAddOperationToGroupForRootResource() {
        JavaJAXRSSpecServerCodegen instance = new JavaJAXRSSpecServerCodegen();
        
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "findArbeitsstoffe";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Arbeitsstoff", "/", operation, codegenOperation, operationList);
        
        assertThat(operationList.size(), is(1));
        assertThat(operationList.containsKey(""), is(true));
        assertThat(codegenOperation.baseName, is("Arbeitsstoff"));
    }
    
    /**
     * Test
     * {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path param.
     */
    @Test
    public void testAddOperationToGroupForRootResourcePathParam() {
        JavaJAXRSSpecServerCodegen instance = new JavaJAXRSSpecServerCodegen();
        
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "getArbeitsstoff";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Arbeitsstoff", "/{uuid}", operation, codegenOperation, operationList);
        
        assertThat(operationList.size(), is(1));
        assertThat(operationList.containsKey(""), is(true));
        assertThat(codegenOperation.baseName, is("Arbeitsstoff"));
    }
    
    /**
     * Test
     * {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String,
     * Operation, CodegenOperation, Map)} for Resource with path "/subresource".
     */
    @Test
    public void testAddOperationToGroupForSubresource() {
        JavaJAXRSSpecServerCodegen instance = new JavaJAXRSSpecServerCodegen();
        
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.path = "/subresource";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Default", "/subresource", operation, codegenOperation, operationList);
        
        assertThat(codegenOperation.baseName, is("subresource"));
        assertThat(operationList.size(), is(1));
        assertThat(operationList.containsKey("subresource"), is(true));
    }
}
