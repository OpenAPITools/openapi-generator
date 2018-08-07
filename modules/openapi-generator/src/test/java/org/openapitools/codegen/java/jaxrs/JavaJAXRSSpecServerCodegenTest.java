package org.openapitools.codegen.java.jaxrs;

import io.swagger.v3.oas.models.Operation;
import org.junit.Ignore;
import org.junit.Test;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen;

import java.util.Collections;
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
     * {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path "/".
     */
    @Test
    @Ignore
    public void testAddOperationToGroupForRootResource() {
        JavaJAXRSSpecServerCodegen instance = new JavaJAXRSSpecServerCodegen();
        
        CodegenOperation codegenOperation = new CodegenOperation();
        
        instance.addOperationToGroup("Arbeitsstoff", "/", null, codegenOperation, Collections.emptyMap());
        
        System.out.println(codegenOperation.baseName);
    }
    
    /**
     * Test
     * {@link org.openapitools.codegen.languages.JavaJAXRSSpecServerCodegen#addOperationToGroup(String, String, Operation, CodegenOperation, Map)} for Resource with path "/subresource".
     */
    @Test
    public void testAddOperationToGroupForSubresource() {
        JavaJAXRSSpecServerCodegen instance = new JavaJAXRSSpecServerCodegen();
        
        CodegenOperation codegenOperation = new CodegenOperation();
        codegenOperation.path = "/subresource";
        Map<String, List<CodegenOperation>> operationList = new HashMap<>();
        
        instance.addOperationToGroup("Subresource", "/subresource", null, codegenOperation, operationList);
        
        assertThat(codegenOperation.baseName, is("subresource"));
        assertThat(operationList.size(), is(1));
    }
}
