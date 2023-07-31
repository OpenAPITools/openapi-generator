package org.openapitools;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.assertj.core.api.Assertions;
import org.json.JSONException;
import org.junit.jupiter.api.Test;
import org.openapitools.model.ResponseObjectWithDifferentFieldNames;
import org.skyscreamer.jsonassert.JSONAssert;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
public class ResponseObjectWithDifferentFieldNamesJsonSerializationTest {

    private static final String JSON_STRING = "{" +
        "\"normalPropertyName\": \"normalPropertyName_value\"," +
        "\"lower-case-property-dashes\": \"lowerCasePropertyDashes_value\"," +
        "\"property name with spaces\": \"propertyNameWithSpaces_value\"," +
        "\"UPPER_CASE_PROPERTY_SNAKE\": \"UPPER_CASE_PROPERTY_SNAKE_value\"" +
        "}";

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldSerializedModelToJson() throws JsonProcessingException, JSONException {
        ResponseObjectWithDifferentFieldNames object = new ResponseObjectWithDifferentFieldNames()
            .normalPropertyName("normalPropertyName_value")
            .UPPER_CASE_PROPERTY_SNAKE("UPPER_CASE_PROPERTY_SNAKE_value")
            .lowerCasePropertyDashes("lowerCasePropertyDashes_value")
            .propertyNameWithSpaces("propertyNameWithSpaces_value");

        String actualJson = objectMapper.writeValueAsString(object);

        JSONAssert.assertEquals(JSON_STRING, actualJson, true);
    }

    @Test
    void shouldDeserializedJsonToModel() throws JsonProcessingException {
        String json = "{" +
            "\"normalPropertyName\": \"normalPropertyName_value\"," +
            "\"lower-case-property-dashes\": \"lowerCasePropertyDashes_value\"," +
            "\"property name with spaces\": \"propertyNameWithSpaces_value\"," +
            "\"UPPER_CASE_PROPERTY_SNAKE\": \"UPPER_CASE_PROPERTY_SNAKE_value\"" +
            "}";

        ResponseObjectWithDifferentFieldNames object = objectMapper.readValue(json, ResponseObjectWithDifferentFieldNames.class);

        Assertions.assertThat(object)
            .returns("normalPropertyName_value", ResponseObjectWithDifferentFieldNames::getNormalPropertyName)
            .returns("UPPER_CASE_PROPERTY_SNAKE_value", ResponseObjectWithDifferentFieldNames::getUPPERCASEPROPERTYSNAKE)
            .returns("lowerCasePropertyDashes_value", ResponseObjectWithDifferentFieldNames::getLowerCasePropertyDashes)
            .returns("propertyNameWithSpaces_value", ResponseObjectWithDifferentFieldNames::getPropertyNameWithSpaces);
    }

}
