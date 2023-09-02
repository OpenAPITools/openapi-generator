package org.openapitools;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.openapitools.model.Pet;
import org.springframework.boot.test.context.SpringBootTest;

@SpringBootTest
class OpenApiGeneratorApplicationTests {

    @Test
    void contextLoads() {
    }

    @Test
    void shouldDeserializeWithoutLoss() throws JsonProcessingException {
        // Given
        Long petId = 5L;
        String petName = "Rex";
        List<String> photoUrls = Collections.singletonList("url1");
        Pet pet = new Pet(petName, photoUrls).id(petId);

        // When
        ObjectMapper mapper = new ObjectMapper();
        Pet deserializedPet = mapper.readValue(mapper.writeValueAsString(pet), Pet.class);

        // Then
        assertThat(deserializedPet)
                .isNotNull()
                .hasFieldOrPropertyWithValue("name", petName)
                .hasFieldOrPropertyWithValue("photoUrls", photoUrls)
                .hasFieldOrPropertyWithValue("id", petId)
                .hasFieldOrPropertyWithValue("category", null);
    }
}