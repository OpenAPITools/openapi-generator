package org.openapitools;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.openapitools.model.Pet;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;

@SpringBootTest
class JacksonTest {

    @Test
    void shouldSerializeToXml() throws JsonProcessingException {
        // Given
        XmlMapper mapper = new XmlMapper();
        Pet pet = new Pet()
                .name("Red")
                .status(Pet.StatusEnum.AVAILABLE);

        // When
        String xmlPet = mapper.writeValueAsString(pet);
        Pet deserializedPet = mapper.readValue(xmlPet, Pet.class);

        // Then
        assertThat(deserializedPet)
                .isNotNull()
                .returns("Red", Pet::getName);
    }
}