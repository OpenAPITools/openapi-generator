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
    void shouldDeserializeAnimal() throws JsonProcessingException {
        // Given
        AnimalDto animal = new AnimalDto()
                .color("brown");
        AnimalDto deserializedPet = mapper.readValue(mapper.writeValueAsString(animal), AnimalDto.class);

        // Then
        assertThat(deserializedPet)
                .isNotNull()
                .returns("Animal", AnimalDto::getClassName)
                .returns("brown", AnimalDto::getColor);
    }

    @Test
    void shouldDeserializeWithoutLoss() throws JsonProcessingException {
        // Given
        CatDto animal = new CatDto()
            .color("Red");

        // When
        AnimalDto deserializedPet = mapper.readValue(mapper.writeValueAsString(animal), AnimalDto.class);

        // Then
        assertThat(deserializedPet)
                .isNotNull()
                .returns("Cat", AnimalDto::getClassName)
                .returns("Red", AnimalDto::getColor);
    }

    @Test
    void shouldDeserializeObjectWithInheritanceWithoutLoss() throws JsonProcessingException {
        // Given
        String json = "{\"className\":\"BigCat\",\"color\":\"red\",\"declawed\":null,\"kind\":\"tigers\"}";

        // When
        AnimalDto deserializedAnimal = mapper.readValue(json, AnimalDto.class);

        // Then
        assertThat(deserializedAnimal).isInstanceOf(BigCatDto.class);
        BigCatDto deserializedCat = (BigCatDto)deserializedAnimal;

        assertThat(deserializedCat)
                .isNotNull()
                .returns("BigCat", AnimalDto::getClassName)
                .returns("red", AnimalDto::getColor)
                .returns(BigCatDto.KindEnum.TIGERS, BigCatDto::getKind);
    }
}
