package org.openapitools.codegen.plantuml;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.PlantumlDocumentationCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.*;

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

        Map<String, Object> objs = createObjectsMapFor(cm);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        List<Object> entityList = getList(objs, "entities");
        Assert.assertFalse(entityList.isEmpty(), "empty entity list");

        Map<String, Object> firstEntity = getEntityFromList("Sample", entityList);

        List<Object> fieldList = getList(firstEntity, "fields");
        Assert.assertEquals(fieldList.size(), 2, "size of field list");

        Map<String, Object> firstField = (Map<String, Object>)fieldList.get(0);

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

        Map<String, Object> objs = createObjectsMapFor(cm);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        List<Object> entityList = getList(objs, "entities");

        Map<String, Object> sampleEntity = getEntityFromList("Sample", entityList);
        Map<String, Object> tagsField = getFieldFromEntity("tags", sampleEntity);
        Assert.assertEquals((String)tagsField.get("dataType"), "List<String>");
    }

    @Test
    public void inheritedEntitiesTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema parentSchema = new Schema()
                .description("a parent model")
                .addProperties("id", new StringSchema())
                .addRequiredItem("id");

        openAPI.getComponents().addSchemas("parent", parentSchema);

        final Schema childAllOfInlineSchema = new Schema()
                .description("an inline model")
                .addProperties("name", new StringSchema());

        openAPI.getComponents().addSchemas("child_allOf", childAllOfInlineSchema);

        final ComposedSchema composedSchema = new ComposedSchema();
        composedSchema.setDescription("a composed child model");
        composedSchema.addAllOfItem(new Schema().$ref("#/components/schemas/parent"));
        composedSchema.addAllOfItem(new Schema().$ref("#/components/schemas/child_allOf"));
        openAPI.getComponents().addSchemas("child", composedSchema);

        plantumlDocumentationCodegen.setOpenAPI(openAPI);
        final CodegenModel parentModel = plantumlDocumentationCodegen.fromModel("parent", parentSchema);
        final CodegenModel childAllOfModel = plantumlDocumentationCodegen.fromModel("child_allOf", childAllOfInlineSchema);
        final CodegenModel childComposedModel = plantumlDocumentationCodegen.fromModel("child", composedSchema);

        Map<String, Object> objs = createObjectsMapFor(parentModel, childComposedModel, childAllOfModel);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        List<Object> entityList = getList(objs, "entities");
        Assert.assertEquals(entityList.size(), 2, "size of entity list");

        assertEntityDoesNotExistsInList("ChildAllOf", entityList);

        Map<String, Object> parentEntity = getEntityFromList("Parent", entityList);
        getFieldFromEntity("id", parentEntity);

        Map<String, Object> childEntity = getEntityFromList("Child", entityList);
        assertFieldDoesNotExistsInEntity("id", childEntity);
        getFieldFromEntity("name", childEntity);

        List<Object> inheritanceList = getList(objs, "inheritances");
        Assert.assertEquals(inheritanceList.size(), 1, "size of inheritance list");

        Map<String, String> firstInheritance = (Map<String, String>)inheritanceList.get(0);
        Assert.assertEquals(firstInheritance.get("parent"), "Parent");
        Assert.assertEquals(firstInheritance.get("child"), "Child");
    }

    @Test
    public void aggregatedEntitiesTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema simpleDataTypeSchema = new Schema()
                .description("a simple model")
                .addProperties("name", new StringSchema());

        openAPI.getComponents().addSchemas("simple", simpleDataTypeSchema);

        final Schema tagDataTypeSchema = new Schema()
                .description("a tag model")
                .addProperties("name", new StringSchema());

        openAPI.getComponents().addSchemas("tag", tagDataTypeSchema);

        final Schema parentSchema = new Schema()
                .description("a parent model")
                .addProperties("id", new StringSchema())
                .addProperties("name", new Schema().$ref("#/components/schemas/simple"))
                .addProperties("tags", new ArraySchema().items(new Schema().$ref("#/components/schemas/tag")))
                .addRequiredItem("id");

        openAPI.getComponents().addSchemas("parent", parentSchema);

        plantumlDocumentationCodegen.setOpenAPI(openAPI);
        final CodegenModel simpleModel = plantumlDocumentationCodegen.fromModel("simple", simpleDataTypeSchema);
        final CodegenModel tagModel = plantumlDocumentationCodegen.fromModel("tag", tagDataTypeSchema);
        final CodegenModel parentModel = plantumlDocumentationCodegen.fromModel("parent", parentSchema);

        Map<String, Object> objs = createObjectsMapFor(parentModel, simpleModel, tagModel);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        List<Object> entityList = getList(objs, "entities");
        Assert.assertEquals(entityList.size(), 3, "size of entity list");

        Map<String, Object> parentEntity = getEntityFromList("Parent", entityList);

        Map<String, Object> nameField = getFieldFromEntity("name", parentEntity);
        Assert.assertEquals((String)nameField.get("dataType"), "Simple");

        Map<String, Object> tagsField = getFieldFromEntity("tags", parentEntity);
        Assert.assertEquals((String)tagsField.get("dataType"), "List<Tag>");

        List<Object> relationshipList = getList(objs, "relationships");
        Assert.assertEquals(relationshipList.size(), 2, "size of relationship list");

        Map<String, Object> firstRelationship = (Map<String, Object>)relationshipList.get(0);
        Assert.assertEquals((String)firstRelationship.get("parent"), "Parent");
        Assert.assertEquals((String)firstRelationship.get("child"), "Simple");
        Assert.assertEquals((String)firstRelationship.get("name"), "name");
        Assert.assertFalse((boolean)firstRelationship.get("isList"));

        Map<String, Object> secondRelationship = (Map<String, Object>)relationshipList.get(1);
        Assert.assertEquals((String)secondRelationship.get("parent"), "Parent");
        Assert.assertEquals((String)secondRelationship.get("child"), "Tag");
        Assert.assertEquals((String)secondRelationship.get("name"), "tags");
        Assert.assertTrue((boolean)secondRelationship.get("isList"));
    }

    @Test
    public void sharedIdenticalInlineAllOfTest() {
        OpenAPI openAPI = TestUtils.createOpenAPI();
        final Schema parentSchema = new Schema()
                .description("a parent model")
                .addProperties("id", new StringSchema());

        openAPI.getComponents().addSchemas("parent", parentSchema);

        final Schema tagDataTypeSchema = new Schema()
                .description("a tag model")
                .addProperties("name", new StringSchema());

        openAPI.getComponents().addSchemas("tag", tagDataTypeSchema);

        final Schema anotherAllOfInlineSchema = new Schema()
                .description("an identical inline model used elsewhere")
                .addProperties("tag", new Schema().$ref("#/components/schemas/tag"));

        openAPI.getComponents().addSchemas("another_allOf", anotherAllOfInlineSchema);

        final ComposedSchema composedSchema = new ComposedSchema();
        composedSchema.setDescription("a composed child model");
        composedSchema.addAllOfItem(new Schema().$ref("#/components/schemas/parent"));
        composedSchema.addAllOfItem(new Schema().$ref("#/components/schemas/another_allOf"));
        openAPI.getComponents().addSchemas("child", composedSchema);

        plantumlDocumentationCodegen.setOpenAPI(openAPI);
        final CodegenModel parentModel = plantumlDocumentationCodegen.fromModel("parent", parentSchema);
        final CodegenModel childAllOfModel = plantumlDocumentationCodegen.fromModel("another_allOf", anotherAllOfInlineSchema);
        final CodegenModel childComposedModel = plantumlDocumentationCodegen.fromModel("child", composedSchema);

        Map<String, Object> objs = createObjectsMapFor(parentModel, childComposedModel, childAllOfModel);

        plantumlDocumentationCodegen.postProcessSupportingFileData(objs);

        List<Object> entityList = getList(objs, "entities");
        Assert.assertEquals(entityList.size(), 2, "size of entity list");

        Map<String, Object> parentEntity = getEntityFromList("Parent", entityList);
        Map<String, Object> childEntity = getEntityFromList("Child", entityList);

        List<Object> inheritanceList = getList(objs, "inheritances");
        Assert.assertEquals(inheritanceList.size(), 1, "size of inheritance list");

        Map<String, String> firstInheritance = (Map<String, String>)inheritanceList.get(0);
        Assert.assertEquals(firstInheritance.get("parent"), "Parent");
        Assert.assertEquals(firstInheritance.get("child"), "Child");

        List<Object> relationshipList = getList(objs, "relationships");
        Assert.assertEquals(relationshipList.size(), 1, "size of relationship list");

        Map<String, Object> firstRelationship = (Map<String, Object>)relationshipList.get(0);
        Assert.assertEquals((String)firstRelationship.get("parent"), "Child");
        Assert.assertEquals((String)firstRelationship.get("child"), "Tag");
    }

    private Map<String, Object> createObjectsMapFor(CodegenModel... codegenModels) {
        List<Map<?,?>> modelsList = new ArrayList();

        for (CodegenModel codegenModel: codegenModels) {
            Map<String, Object> modelMap = new HashMap<>();
            modelMap.put("model", codegenModel);
            modelsList.add(modelMap);
        }

        Map<String, Object> objs = new HashMap<>();
        objs.put("models", modelsList);
        return objs;
    }

    private Map<String, Object> toMap(Object entityItem) {
        return (Map<String, Object>)entityItem;
    }

    private boolean hasName(String name, Map<String, Object> map) {
        return (map.get("name")).equals(name);
    }

    private void assertEntityDoesNotExistsInList(String name, List<?> entityList) {
        long count = entityList.stream()
                .map(entityItem -> toMap(entityItem))
                .filter(entityMap -> hasName(name, entityMap))
                .count();

        Assert.assertEquals(count, 0, "entries with name " + name);
    }

    private Map<String, Object> getEntityFromList(String name, List<?> entityList) {
        Optional<Map<String, Object>> entity = entityList.stream()
                .map(entityItem -> toMap(entityItem))
                .filter(entityMap -> hasName(name, entityMap))
                .findFirst();

        Assert.assertTrue(entity.isPresent(), "entity with name '" + name + "' found in list");

        return entity.get();
    }

    private void assertFieldDoesNotExistsInEntity(String name, Map<String, Object> entity) {
        List<Object> fieldList = (List<Object>)entity.get("fields");
        long count = fieldList.stream()
                .map(fieldItem -> toMap(fieldItem))
                .filter(fieldMap -> hasName(name, fieldMap))
                .count();

        Assert.assertEquals(count, 0, "fields with name " + name);
    }

    private Map<String, Object> getFieldFromEntity(String name, Map<String, Object> entity) {
        List<Object> fieldList = (List<Object>)entity.get("fields");
        Optional<Map<String, Object>> field = fieldList.stream()
                .map(fieldItem -> toMap(fieldItem))
                .filter(fieldMap -> hasName(name, fieldMap))
                .findFirst();

        Assert.assertTrue(field.isPresent(), "field with name '" + name + "' found in list");

        return field.get();
    }

    private List<Object> getList(Map<String, Object> objs, String listName) {
        Object list = objs.get(listName);
        Assert.assertNotNull(list, "object with name '" + listName + "' in objs map");

        Assert.assertTrue(list instanceof List<?>, "object with name '" + listName + "' in objs map is a list");
        return (List<Object>)list;
    }
}
