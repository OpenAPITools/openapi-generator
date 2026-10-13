package org.openapitools.codegen.languages;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.io.FileUtils;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultCodegen;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.slf4j.LoggerFactory;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class SpringPageableOptionsTest {

    @DataProvider
    public Object[][] generators() {
        return new Object[][] {
                {false, "spring-boot"}, {true, "spring-boot"},
                {false, "spring-cloud"}, {true, "spring-cloud"}
        };
    }

    private DefaultCodegen generator(boolean kotlin, String library) {
        DefaultCodegen generator = kotlin ? new KotlinSpringServerCodegen() : new SpringCodegen();
        generator.setLibrary(library);
        generator.additionalProperties().put("interfaceOnly", true);
        generator.additionalProperties().put("skipDefaultInterface", true);
        generator.additionalProperties().put("useTags", true);
        generator.additionalProperties().put("useSpringBoot3", true);
        generator.additionalProperties().put("generatePageableConstraintValidation", true);
        generator.additionalProperties().put("generateSortValidation", true);
        return generator;
    }

    private OpenAPI spec() {
        return TestUtils.parseSpec("src/test/resources/3_0/spring/pageable-one-indexed.yaml");
    }

    private Schema<?> page(OpenAPI spec) {
        return spec.getPaths().get("/items").getGet().getParameters().get(0).getSchema();
    }

    private String generate(DefaultCodegen generator, OpenAPI spec, boolean kotlin,
            boolean expectValidator) throws IOException {
        Path output = Files.createTempDirectory("pageable-options");
        generator.setOutputDir(output.toString());
        try {
            List<File> files = new DefaultGenerator().opts(new ClientOptInput().config(generator).openAPI(spec)).generate();
            assertThat(files.stream().anyMatch(file -> file.getName().equals(kotlin ? "ValidPageable.kt" : "ValidPageable.java")))
                    .isEqualTo(expectValidator);
            File api = files.stream().filter(file -> file.getName().equals(kotlin ? "ItemsApi.kt" : "ItemsApi.java"))
                    .findFirst().orElseThrow();
            return Files.readString(api.toPath());
        } finally {
            FileUtils.deleteDirectory(output.toFile());
        }
    }

    @Test(dataProvider = "generators")
    public void implicitIndexingPreservesExistingOutput(boolean kotlin, String library) throws IOException {
        String api = generate(generator(kotlin, library), spec(), kotlin, true);
        assertThat(api).contains("@PageableDefault(page = 1, size = 20)",
                "@ValidPageable(maxSize = 100, maxPage = 5, minSize = 1, minPage = 1)", "@SortDefault", "@ValidSort");
    }

    @Test(dataProvider = "generators")
    public void oneIndexedDefaultsAndBoundsAreNormalized(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("oneIndexedPageParameters", "true");
        String api = generate(generator, spec(), kotlin, true);
        assertThat(api).contains("@PageableDefault(page = 0, size = 20)",
                "@ValidPageable(maxSize = 100, maxPage = 4, minSize = 1, minPage = 0)", "@ValidSort");
        assertThat(api).contains(kotlin
                ? "@SortDefault.SortDefaults(SortDefault(sort = [\"name\"], direction = Sort.Direction.DESC))"
                : "@SortDefault.SortDefaults({@SortDefault(sort = {\"name\"}, direction = Sort.Direction.DESC)})");
    }

    @Test(dataProvider = "generators")
    public void disablingDefaultsPreservesValidation(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("generatePageableDefaults", "false");
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        OpenAPI spec = spec();
        page(spec).setDefault(0);
        String api = generate(generator, spec, kotlin, true);
        assertThat(api).contains("@ValidPageable(maxSize = 100, maxPage = 4, minSize = 1, minPage = 0)", "@ValidSort");
        assertThat(api).doesNotContain("PageableDefault", "SortDefault", "import org.springframework.data.domain.Sort");
    }

    @Test(dataProvider = "generators")
    public void autoDetectedPaginationUsesIndexingOption(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("autoXSpringPaginated", "page-size");
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        OpenAPI spec = spec();
        spec.getPaths().get("/items").getGet().getExtensions().clear();
        assertThat(generate(generator, spec, kotlin, true)).contains("@PageableDefault(page = 0, size = 20)");
    }

    @Test(dataProvider = "generators")
    public void fractionalSizeBoundsAreNormalized(boolean kotlin, String library) throws IOException {
        for (boolean oneIndexed : List.of(false, true)) {
            for (boolean exclusive : List.of(false, true)) {
                DefaultCodegen generator = generator(kotlin, library);
                generator.additionalProperties().put("oneIndexedPageParameters", oneIndexed);
                OpenAPI spec = spec();
                Schema<?> size = spec.getPaths().get("/items").getGet().getParameters().get(1).getSchema();
                size.setMinimum(new BigDecimal("1.5"));
                size.setMaximum(new BigDecimal("100.5"));
                size.setExclusiveMinimum(exclusive);
                size.setExclusiveMaximum(exclusive);
                assertThat(generate(generator, spec, kotlin, true)).contains(
                        "@ValidPageable(maxSize = 100, maxPage = " + (oneIndexed ? 4 : 5)
                                + ", minSize = 2, minPage = " + (oneIndexed ? 0 : 1) + ")");
            }
        }
    }

    @Test(dataProvider = "generators")
    public void fractionalDefaultsFailOnlyWhenEmitted(boolean kotlin, String library) throws IOException {
        for (int parameterIndex : List.of(0, 1)) {
            OpenAPI spec = spec();
            spec.getPaths().get("/items").getGet().getParameters().get(parameterIndex)
                    .setSchema(new io.swagger.v3.oas.models.media.NumberSchema()._default(new BigDecimal("1.5")));
            DefaultCodegen enabled = generator(kotlin, library);
            assertThatThrownBy(() -> generate(enabled, spec, kotlin, true))
                    .hasStackTraceContaining(parameterIndex == 0 ? "page default 1.5" : "size default 1.5")
                    .hasStackTraceContaining("listItems");
            DefaultCodegen disabled = generator(kotlin, library);
            disabled.additionalProperties().put("generatePageableDefaults", false);
            assertThat(generate(disabled, spec, kotlin, true)).doesNotContain("@PageableDefault");
        }
    }

    @Test(dataProvider = "generators")
    public void disabledConstraintValidationDoesNotResolveOverflowingBounds(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("generatePageableConstraintValidation", false);
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        OpenAPI spec = spec();
        for (int parameterIndex : List.of(0, 1)) {
            spec.getPaths().get("/items").getGet().getParameters().get(parameterIndex).getSchema()
                    .setMaximum(new BigDecimal("2147483648"));
        }
        assertThat(generate(generator, spec, kotlin, false)).doesNotContain("@ValidPageable");
    }

    @Test(dataProvider = "generators")
    public void invalidOneIndexedDefaultFailsGeneration(boolean kotlin, String library) {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        OpenAPI spec = spec();
        page(spec).setDefault(0);
        generator.processOpts();
        assertThatThrownBy(() -> generator.preprocessOpenAPI(spec))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("listItems").hasMessageContaining("page default 0");
    }

    @Test(dataProvider = "generators")
    public void invalidOneIndexedBoundFailsGeneration(boolean kotlin, String library) {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        OpenAPI spec = spec();
        // Exclusive maximum 1 has effective maximum 0, which must not become the absent-bound sentinel.
        page(spec).setMaximum(BigDecimal.ONE);
        page(spec).setExclusiveMaximum(true);
        generator.processOpts();
        assertThatThrownBy(() -> generator.preprocessOpenAPI(spec))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("listItems").hasMessageContaining("effective maximum 0");
    }

    @Test(dataProvider = "generators")
    public void disabledBehaviorsDoNotRejectUnusedValues(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        generator.additionalProperties().put("generatePageableDefaults", false);
        generator.additionalProperties().put("useBeanValidation", false);
        OpenAPI spec = spec();
        page(spec).setDefault(0);
        page(spec).setMaximum(BigDecimal.ZERO);
        page(spec).setMinimum(BigDecimal.ZERO);
        assertThat(generate(generator, spec, kotlin, false))
                .doesNotContain("PageableDefault", "SortDefault", "ValidPageable", "ValidSort");
    }

    @Test(dataProvider = "generators")
    public void disabledConstraintValidationDoesNotRejectUnusedBounds(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("oneIndexedPageParameters", true);
        generator.additionalProperties().put("generatePageableConstraintValidation", false);
        OpenAPI spec = spec();
        page(spec).setMaximum(BigDecimal.ZERO);
        page(spec).setMinimum(BigDecimal.ZERO);
        assertThat(generate(generator, spec, kotlin, false))
                .contains("@PageableDefault(page = 0, size = 20)", "@ValidSort").doesNotContain("ValidPageable");
    }

    @Test(dataProvider = "generators")
    public void explicitZeroIndexingPreservesZeroDefaults(boolean kotlin, String library) throws IOException {
        DefaultCodegen generator = generator(kotlin, library);
        generator.additionalProperties().put("oneIndexedPageParameters", false);
        OpenAPI spec = spec();
        page(spec).setDefault(0);
        page(spec).setMinimum(BigDecimal.ZERO);
        assertThat(generate(generator, spec, kotlin, true))
                .contains("@PageableDefault(page = 0, size = 20)",
                        "@ValidPageable(maxSize = 100, maxPage = 5, minSize = 1, minPage = 0)");
    }

    @Test(dataProvider = "generators")
    public void explicitIndexingMapOrSetterSilencesWarning(boolean kotlin, String library) {
        Logger logger = (Logger) LoggerFactory.getLogger(SpringPageableScanUtils.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        logger.addAppender(appender);
        try {
            for (String configuration : List.of("implicit", "mapFalse", "mapTrue", "setterFalse", "setterTrue")) {
                DefaultCodegen generator = generator(kotlin, library);
                generator.additionalProperties().put("generatePageableDefaults", false);
                if (configuration.startsWith("map")) {
                    generator.additionalProperties().put("oneIndexedPageParameters", configuration.endsWith("True"));
                } else if (configuration.startsWith("setter")) {
                    boolean value = configuration.endsWith("True");
                    if (kotlin) {
                        ((KotlinSpringServerCodegen) generator).setOneIndexedPageParameters(value);
                    } else {
                        ((SpringCodegen) generator).setOneIndexedPageParameters(value);
                    }
                }
                generator.processOpts();
                // Kotlin's spring-cloud processing cannot be repeated once it writes the reactive option back.
                if (!kotlin || !library.equals("spring-cloud")) {
                    generator.processOpts();
                }
                appender.list.clear();
                generator.preprocessOpenAPI(spec());
                assertThat(appender.list.stream().filter(event -> event.getFormattedMessage().contains("has page default 1")).count())
                        .as(configuration).isEqualTo(configuration.equals("implicit") ? 1 : 0);
                if (configuration.equals("implicit")) {
                    assertThat(appender.list).anySatisfy(event -> assertThat(event.getFormattedMessage())
                            .contains("listItems", "zero-based", "oneIndexedPageParameters", "Feign"));
                }
            }
        } finally {
            logger.detachAppender(appender);
            appender.stop();
        }
    }
}
