package org.openapitools.model;

import jakarta.validation.Validator;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.json.JsonMapper;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * The models with additionalProperties are records here: Spring Boot 4 brings Jackson 3, which reads the undeclared
 * properties into a record through {@code @JsonAnySetter} on the constructor parameter. The mapper is Spring's own,
 * which ignores unknown properties, so a missing {@code @JsonAnySetter} would drop the data silently.
 */
@SpringBootTest
class AdditionalPropertiesRecordsTest {

    @Autowired
    private JsonMapper jsonMapper;

    @Autowired
    private Validator validator;

    @Test
    void modelsWithAdditionalPropertiesAreRecords() {
        assertThat(Tagged.class.isRecord()).isTrue();
        assertThat(Loose.class.isRecord()).isTrue();
        assertThat(Typed.class.isRecord()).isTrue();
        assertThat(Lists.class.isRecord()).isTrue();
        assertThat(Holder.class.isRecord()).isTrue();
        assertThat(Child.class.isRecord()).as("has a parent").isFalse();
    }

    @Test
    void undeclaredPropertiesAreReadIntoTheMapInAnyPosition() throws Exception {
        Tagged first = jsonMapper.readValue("{\"extra\":\"a\",\"name\":\"n\",\"other\":\"b\"}", Tagged.class);

        assertThat(first.name()).isEqualTo("n");
        assertThat(first.additionalProperties()).containsOnly(Map.entry("extra", "a"), Map.entry("other", "b"));
        assertThat(first.getAdditionalProperty("extra")).isEqualTo("a");
        assertThat(first.getAdditionalProperty("missing")).isNull();
    }

    @Test
    void anAbsentMapIsEmptyNotNull() throws Exception {
        Tagged tagged = jsonMapper.readValue("{\"name\":\"n\"}", Tagged.class);

        assertThat(tagged.additionalProperties()).isEmpty();
        assertThat(tagged).isEqualTo(new Tagged("n", null, null, null, null));
        assertThat(tagged.count()).as("the default of the compact constructor").isEqualTo(3);
    }

    @Test
    void theMapIsWrittenAsTopLevelProperties() throws Exception {
        Tagged tagged = Tagged.builder().name("n").additionalProperties(Map.of("extra", "a")).build();

        JsonNode json = jsonMapper.readTree(jsonMapper.writeValueAsString(tagged));

        assertThat(json.get("extra").toString()).isEqualTo("\"a\"");
        assertThat(json.has("additionalProperties")).isFalse();
        assertThat(json.get("name").toString()).isEqualTo("\"n\"");
    }

    @Test
    void valuesOfAnyTypeAndOfModelTypeKeepTheirShape() throws Exception {
        Loose loose = jsonMapper.readValue("{\"id\":\"i\",\"o\":{\"k\":[1,2]},\"n\":null}", Loose.class);
        Typed typed = jsonMapper.readValue("{\"id\":\"i\",\"e\":{\"value\":\"v\"}}", Typed.class);
        Lists lists = jsonMapper.readValue("{\"id\":\"i\",\"l\":[1,2]}", Lists.class);

        assertThat(loose.additionalProperties()).containsKeys("o", "n");
        assertThat(loose.additionalProperties().get("o")).isEqualTo(Map.of("k", List.of(1, 2)));
        assertThat(typed.additionalProperties().get("e")).isEqualTo(new Item("v"));
        assertThat(lists.additionalProperties().get("l")).isEqualTo(List.of(1, 2));
    }

    @Test
    void equalityToStringAndTheBuilderCoverTheMap() throws Exception {
        Tagged read = jsonMapper.readValue("{\"name\":\"n\",\"secret\":\"p\",\"x\":\"1\"}", Tagged.class);
        Tagged built = Tagged.builder().name("n").secret("p").additionalProperties(Map.of("x", "1")).build();

        assertThat(read).isEqualTo(built).hasSameHashCodeAs(built);
        assertThat(built.toBuilder().build()).isEqualTo(built);
        assertThat(read.toString()).contains("additionalProperties={x=1}").contains("secret=*").doesNotContain("secret=p");
    }

    @Test
    void aNestedRecordKeepsItsOwnMap() throws Exception {
        Holder holder = jsonMapper.readValue("{\"tagged\":{\"name\":\"n\",\"x\":\"1\"},\"loose\":{\"y\":true}}", Holder.class);

        assertThat(holder.tagged().additionalProperties()).containsOnly(Map.entry("x", "1"));
        assertThat(holder.loose().additionalProperties()).containsOnly(Map.entry("y", true));
    }

    @Test
    void validationStillAppliesToTheDeclaredComponents() throws Exception {
        Tagged missingName = jsonMapper.readValue("{\"x\":\"1\"}", Tagged.class);

        assertThat(validator.validate(missingName)).extracting(v -> v.getPropertyPath().toString()).containsExactly("name");
    }
}
