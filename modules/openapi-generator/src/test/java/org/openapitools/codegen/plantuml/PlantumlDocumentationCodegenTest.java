package org.openapitools.codegen.plantuml;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.PlantumlDocumentationCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PlantumlDocumentationCodegenTest {
    private PlantumlDocumentationCodegen plantumlDocumentationCodegen = new PlantumlDocumentationCodegen();

    @Test
    public void simpleEntityTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("id", new IntegerSchema().format(SchemaTypeUtil.INTEGER64_FORMAT))
                .addProperties("name", new StringSchema())
                .addRequiredItem("id")
                .addRequiredItem("name");
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        plantumlDocumentationCodegen.setOpenAPI(openAPI);
        final CodegenModel cm = plantumlDocumentationCodegen.fromModel("sample", model);

        Map<String, Object> modelsMap = new HashMap<>();
        modelsMap.put("model", cm);
        List<Map<?,?>> modelsList = new ArrayList();
        modelsList.add(modelsMap);
        Map<String, Object> objs = new HashMap<>();
        objs.put("models", modelsList);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        Object entities = objs.get("entities");
        Assert.assertNotNull(entities);

        Assert.assertTrue(entities instanceof List<?>);
        List<?> entityList = (List<?>)entities;

        Assert.assertFalse(entityList.isEmpty(), "empty entity list");
        Object firstEntityItem = entityList.get(0);

        Assert.assertTrue(firstEntityItem instanceof HashMap<?, ?>, "first item in the entity list is a HashMap");
        HashMap<String, Object> firstEntity = (HashMap<String, Object>)firstEntityItem;

        Assert.assertEquals(firstEntity.get("name"), "Sample");

        List<Object> fieldList = (List<Object>)firstEntity.get("fields");
        Assert.assertEquals(fieldList.size(), 2, "size of field list");

        HashMap<String, Object> firstField = (HashMap<String, Object>)fieldList.get(0);

        Assert.assertEquals((String)firstField.get("name"), "id");
        Assert.assertTrue((boolean)firstField.get("isRequired"));
        Assert.assertEquals((String)firstField.get("dataType"), "Long");
    }

    @Test
    public void listFieldTest() {
        final Schema model = new Schema()
                .description("a sample model")
                .addProperties("tags", new ArraySchema().items(new StringSchema()));
        OpenAPI openAPI = TestUtils.createOpenAPIWithOneSchema("sample", model);
        plantumlDocumentationCodegen.setOpenAPI(openAPI);
        final CodegenModel cm = plantumlDocumentationCodegen.fromModel("sample", model);

        Map<String, Object> modelsMap = new HashMap<>();
        modelsMap.put("model", cm);
        List<Map<?,?>> modelsList = new ArrayList();
        modelsList.add(modelsMap);
        Map<String, Object> objs = new HashMap<>();
        objs.put("models", modelsList);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        Object entities = objs.get("entities");
        List<?> entityList = (List<?>)entities;
        HashMap<String, Object> firstEntity = (HashMap<String, Object>)entityList.get(0);
        List<Object> fieldList = (List<Object>)firstEntity.get("fields");
        HashMap<String, Object> firstField = (HashMap<String, Object>)fieldList.get(0);

        Assert.assertEquals((String)firstField.get("name"), "tags");
        Assert.assertEquals((String)firstField.get("dataType"), "List<String>");
    }
}
