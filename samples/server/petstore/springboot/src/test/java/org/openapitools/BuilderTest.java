package org.openapitools;

import org.junit.jupiter.api.Test;
import org.openapitools.jackson.nullable.JsonNullable;
import org.openapitools.model.BigCatDto;
import org.openapitools.model.ParentWithNullableDto;

import static org.assertj.core.api.Assertions.assertThat;

public class BuilderTest {

    BigCatDto bigCat = BigCatDto.builder()
            .kind(BigCatDto.KindEnum.TIGERS)
            .color("yellow")
            .declawed(false)
            .build();
    @Test
    void builderTest() {
        assertThat(bigCat.getKind()).isEqualTo(BigCatDto.KindEnum.TIGERS);
        assertThat(bigCat.getColor()).isEqualTo("yellow");
        assertThat(bigCat.getDeclawed()).isEqualTo(Boolean.FALSE);
    }

    @Test
    void toBuilderTest() {
        BigCatDto otherBigCat = bigCat.toBuilder()
                .color("orange")
                .build();

        assertThat(otherBigCat).isEqualTo(new BigCatDto()
                .kind(BigCatDto.KindEnum.TIGERS)
                .color("orange")
                .declawed(false));

    }

    @Test
    void jsonNullableTest() {
        ParentWithNullableDto dto = ParentWithNullableDto.builder()
                .nullableProperty("prop")
                .build();

        ParentWithNullableDto expected = new ParentWithNullableDto();
        expected.setNullableProperty(JsonNullable.of("prop"));
        assertThat(dto).isEqualTo(expected);
    }

}
