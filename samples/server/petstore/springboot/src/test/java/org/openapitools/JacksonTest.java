package org.openapitools;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.openapitools.model.AnimalDto;
import org.openapitools.model.BigCatDto;
import org.openapitools.model.CatDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

@SpringBootTest
class JacksonTest {

    @Autowired
    private ObjectMapper mapper;

    @Test
    void shouldDeserializeWithoutLoss() throws JsonProcessingException {
        // Given
        CatDto animal = new CatDto()
            .color("Red");

        // When
        CatDto deserializedPet = mapper.readValue(mapper.writeValueAsString(animal), CatDto.class);

        // Then
        assertThat(deserializedPet)
                .isNotNull()
                .returns("CatDto", AnimalDto::getClassName)
                .returns("Red", AnimalDto::getColor);
    }

    @Test
    void shouldDeserializeObjectWithInheritanceWithoutLoss() throws JsonProcessingException {
        // Given
        String json = "{\"className\":\"BigCat\",\"color\":\"red\",\"declawed\":null,\"kind\":\"tigers\"}";

        // When
        BigCatDto deserializedCat = mapper.readValue(json, BigCatDto.class);

        // Then
        assertThat(deserializedCat)
                .isNotNull()
                .returns("BigCat", AnimalDto::getClassName)
                .returns("red", AnimalDto::getColor)
                .returns(BigCatDto.KindEnum.TIGERS, BigCatDto::getKind);
    }
}