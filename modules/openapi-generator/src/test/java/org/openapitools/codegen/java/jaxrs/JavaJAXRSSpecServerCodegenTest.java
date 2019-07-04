package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.Operation;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;
import org.testng.Assert;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Unit-Test for {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen}.
 *
 * @author attrobit
 */
public class JavaJAXRSSpecServerCodegenTest extends JavaJaxrsBaseTest {

    @BeforeMethod
    public void before() {
        codegen = new JavaJAXRSSpecServerCodegen();
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
        
        codegen.addOperationToGroup("Primaryresource", "/", operation, codegenOperation, operationList);
        
        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey(""));
        Assert.assertEquals(codegenOperation.baseName, "Primaryresource");
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
        
        codegen.addOperationToGroup("Primaryresource", "/{uuid}", operation, codegenOperation, operationList);
        
        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey(""));
        Assert.assertEquals(codegenOperation.baseName, "Primaryresource");
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
        
        codegen.addOperationToGroup("Default", "/subresource", operation, codegenOperation, operationList);
        
        Assert.assertEquals(codegenOperation.baseName, "subresource");
        Assert.assertEquals(operationList.size(), 1);
        Assert.assertTrue(operationList.containsKey("subresource"));
    }
    
    /**
     * Test {@link JavaJAXRSSpecServerCodegen#toApiName(String)} with subresource.
     */
    @Test
    public void testToApiNameForSubresource() {
        final String subresource = codegen.toApiName("subresource");
        Assert.assertEquals(subresource, "SubresourceApi");
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
        codegen.addOperationToGroup("Primaryresource", "/", operation, codegenOperation, operationList);
        
        final String subresource = codegen.toApiName("");
        Assert.assertEquals(subresource, "PrimaryresourceApi");
    }
}
