package org.openapitools.codegen.utils;

import static org.testng.Assert.assertEquals;

import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

public class ProcessUtilsTest {


    @DataProvider(name = "pathMethodValues")
    public static Object[][] dataProvider() {
        return new Object[][]{
                {"/pet", "POST", "PetPost"},
                {"/pet", "PUT", "PetPut"},
                {"/pet/findByStatus", "GET", "PetFindByStatusGet"},
                {"/pet/findByTags", "GET", "PetFindByTagsGet"},
                {"/pet/{petId}", "GET", "PetPetIdGet"},
                {"/pet/{petId}", "POST", "PetPetIdPost"},
                {"/pet/{petId}", "DELETE", "PetPetIdDelete"},
                {"/pet/{petId}/uploadImage", "POST", "PetPetIdUploadImagePost"},
                {"/store/order/{orderId}", "GET", "StoreOrderOrderIdGet"},
                {"/store/order/{orderId}", "DELETE", "StoreOrderOrderIdDelete"},
        };
    }

    @Test(dataProvider = "pathMethodValues")
    public void givenPathAndMethodBuildApiName(String givenPath, String givenMethod, String expected) {
        String result = ProcessUtils.buildApiName(givenPath, givenMethod);

        assertEquals(result, expected);
    }
}