package org.openapitools.codegen.cpprestbed;

import org.jetbrains.annotations.NotNull;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.languages.CppRestbedServerCodegen;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CppRestbedServerTest {

    @DataProvider(name = "providedPaths")
    public Object[][] providedPaths() {
        Object[][] data = new Object[3][2];
        data[0][0] = "/abc/{id}";
        data[0][1] = "/abc/{id: .*}";

        data[1][0] = "/{Foo}/";
        data[1][1] = "/{Foo: .*}/";

        data[2][0] = "xyz/";
        data[2][1] = "xyz/";

        return data;
    }

    @Test(dataProvider = "providedPaths")
    void testPathProcessing(String providedPath, String expectedPath) {
        // Arrange
        CppRestbedServerCodegen codegen = new CppRestbedServerCodegen();
        OperationsMap objs = setupOperationWithPath(providedPath);

        // Act
        OperationsMap processedObjs = codegen.postProcessOperationsWithModels(objs, new ArrayList<>());

        // Assert
        List<CodegenOperation> processedObjsOperationList = extractOperationsList(processedObjs);

        Assert.assertEquals(processedObjsOperationList.size(), 1);
        Assert.assertEquals(processedObjsOperationList.get(0).path, expectedPath);
    }

    private static List<CodegenOperation> extractOperationsList(Map<String, Object> processedObjs) {
        Map<String, Object> processedOperations = (Map<String, Object>) processedObjs.get("operations");
        List<CodegenOperation> processedObjsOperationList = (List<CodegenOperation>) processedOperations.get("operation");
        return processedObjsOperationList;
    }

    @NotNull
    private static OperationsMap setupOperationWithPath(String path) {
        CodegenOperation op = new CodegenOperation();
        op.path = path;

        List<CodegenOperation> operationsList = new ArrayList<>();
        operationsList.add(op);

        OperationMap operations = new OperationMap();
        operations.put("operation", operationsList);

        OperationsMap objs = new OperationsMap();
        objs.put("operations", operations);
        return objs;
    }
}
