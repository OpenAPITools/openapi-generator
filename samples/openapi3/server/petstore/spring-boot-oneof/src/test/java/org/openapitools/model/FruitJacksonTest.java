package org.openapitools.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
class FruitJacksonTest {

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldSerializeAndDeserializeApple() throws JsonProcessingException {
        int seeds = 7;
        Fruit fruit = new Apple(seeds);

        String json = objectMapper.writeValueAsString(fruit);

        assertThat(json).contains("\"fruitType\":\"APPLE\"");

        Fruit result = objectMapper.readValue(json, Fruit.class);

        assertThat(result).isInstanceOfSatisfying(Apple.class, apple -> {
            assertThat(apple.getSeeds()).isEqualTo(seeds);
            assertThat(apple.getFruitType()).isEqualTo(FruitType.APPLE);
        });
    }

    @Test
    void shouldSerializeAndDeserializeBanana() throws JsonProcessingException {
        int length = 7;
        Fruit fruit = new Banana(length);

        String json = objectMapper.writeValueAsString(fruit);

        assertThat(json).contains("\"fruitType\":\"BANANA\"");

        Fruit result = objectMapper.readValue(json, Fruit.class);

        assertThat(result).isInstanceOfSatisfying(Banana.class, banana -> {
            assertThat(banana.getLength()).isEqualTo(length);
            assertThat(banana.getFruitType()).isEqualTo(FruitType.BANANA);
        });
    }

}