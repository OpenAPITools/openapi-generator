package org.openapitools;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;
import org.openapitools.model.Pet;

import static org.assertj.core.api.Assertions.assertThat;

class ForcedGenerateSchemasTest {

    private final ObjectMapper objectMapper = new ObjectMapper();

    @Test
    void mappedAndGeneratedShadowCategoriesHaveEquivalentJson() throws Exception {
        com.example.mapped.Category mapped = new com.example.mapped.Category(1L, "dogs");
        org.openapitools.model.Category shadow = new org.openapitools.model.Category(1L, "dogs");

        assertThat(new Pet().category(mapped).getCategory()).isSameAs(mapped);
        assertThat(objectMapper.readTree(objectMapper.writeValueAsString(mapped)))
                .isEqualTo(objectMapper.readTree(objectMapper.writeValueAsString(shadow)));
    }
}
