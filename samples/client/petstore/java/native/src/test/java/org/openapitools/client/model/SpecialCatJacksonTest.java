package org.openapitools.client.model;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.Test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

public class SpecialCatJacksonTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    public void shouldSerializeAndDeserializeModel() throws JsonProcessingException {
        String color = "yellow-gold";
        Animal model = new SpecialCat().kind(SpecialCat.KindEnum.LIONS).declawed(false).color(color);

        String json = objectMapper.writeValueAsString(model);

        assertThat(json, containsString("\"className\":\"Special-Cat\""));

        Animal result = objectMapper.readValue(json, Animal.class);

        assertThat(result, instanceOf(SpecialCat.class));

        SpecialCat specialCat = (SpecialCat) result;
        assertThat(specialCat.getColor(), is(color));
        assertThat(specialCat.getDeclawed(), is(false));
        assertThat(specialCat.getKind(), is(SpecialCat.KindEnum.LIONS));
    }

}