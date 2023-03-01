package org.openapitools;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.util.Collections;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.openapitools.model.Animal;
import org.openapitools.model.BigCat;
import org.openapitools.model.Cat;
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
        Animal animal = new Animal();

        // When
        ObjectMapper mapper = new ObjectMapper();
        Animal deserializedPet = mapper.readValue(mapper.writeValueAsString(animal), Animal.class);

        // Then
        assertThat(deserializedPet)
                .isNotNull()
                .hasFieldOrPropertyWithValue("className", "Animal");
    }

    @Test
    void shouldDeserializeObjectWithInheritanceWithoutLoss() throws JsonProcessingException {
        // Given
        BigCat cat = new BigCat().kind(BigCat.KindEnum.TIGERS);

        // When
        ObjectMapper mapper = new ObjectMapper();
        BigCat deserializedCat = mapper.readValue(mapper.writeValueAsString(cat), BigCat.class);

        // Then
        assertThat(deserializedCat)
                .isNotNull()
                .hasFieldOrPropertyWithValue("className", "BigCat")
                .hasFieldOrPropertyWithValue("kind", BigCat.KindEnum.TIGERS);
    }
}