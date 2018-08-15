package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.Operation;
import org.junit.Before;
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
    
    private JavaJAXRSSpecServerCodegen instance;
    
    @Before
    public void before() {
        instance = new JavaJAXRSSpecServerCodegen();
    }
    
    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path "/" and set tag.
     */
    @Test
    public void testAddOperationToGroupForRootResource() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "findPrimaryresource";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Primaryresource", "/", operation, codegenOperation, operationList);
        
        assertThat(operationList.size(), is(1));
        assertThat(operationList.containsKey(""), is(true));
        assertThat(codegenOperation.baseName, is("Primaryresource"));
    }
    
    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path param.
     */
    @Test
    public void testAddOperationToGroupForRootResourcePathParam() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "getPrimaryresource";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Primaryresource", "/{uuid}", operation, codegenOperation, operationList);
        
        assertThat(operationList.size(), is(1));
        assertThat(operationList.containsKey(""), is(true));
        assertThat(codegenOperation.baseName, is("Primaryresource"));
    }
    
    /**
     * Test
     * {@link JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String,
     * Operation, CodegenOperation, Map)} for Resource with path "/subresource".
     */
    @Test
    public void testAddOperationToGroupForSubresource() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.path = "/subresource";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Default", "/subresource", operation, codegenOperation, operationList);
        
        assertThat(codegenOperation.baseName, is("subresource"));
        assertThat(operationList.size(), is(1));
        assertThat(operationList.containsKey("subresource"), is(true));
    }
    
    /**
     * Test {@link JavaJAXRSSpecServerCodegen#toApiName(String)} with subresource.
     */
    @Test
    public void testToApiNameForSubresource() {
        final String subresource = instance.toApiName("subresource");
        assertThat(subresource, is("SubresourceApi"));
    }
    
    /**
     * Test {@link JavaJAXRSSpecServerCodegen#toApiName(String)} with primary resource.
     */
    @Test
    public void testToApiNameForPrimaryResource() {
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.operationId = "findPrimaryresource";
        Operation operation = new Operation();
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        instance.addOperationToGroup("Primaryresource", "/", operation, codegenOperation, operationList);
        
        final String subresource = instance.toApiName("");
        assertThat(subresource, is("PrimaryresourceApi"));
    }
}
