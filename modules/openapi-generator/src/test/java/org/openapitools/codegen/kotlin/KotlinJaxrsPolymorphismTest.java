package org.openapitools.codegen.kotlin;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.exc.InvalidTypeIdException;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.languages.KotlinServerCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class KotlinJaxrsPolymorphismTest {
    private static final String SPEC = "src/test/resources/3_0/kotlin/jaxrs-polymorphism.yaml";

    @DataProvider
    public Object[][] inheritanceModes() {
        return new Object[][]{{"abstract", false}, {"none", false}, {"composition", false},
                {"abstract", true}, {"none", true}, {"composition", true}};
    }

    @Test(dataProvider = "inheritanceModes")
    public void generate_WithSupportedInheritanceMode_CompilesKotlinModels(String mode, boolean serializable) throws Exception {
        KotlinServerCodegen codegen = codegen();
        codegen.additionalProperties().put("inheritanceMode", mode);
        codegen.additionalProperties().put("useJakartaEe", false);
        codegen.additionalProperties().put("serializableModel", serializable);

        List<File> files = generate(codegen);
        ClassLoader compiled = compileModels(files);

        assertThat(compiled.loadClass("org.openapitools.server.models.Cat")).isNotNull();
        assertThat(find(files, "AnimalsApi.kt")).content()
                .contains("Uni<Animal?>", "Uni<Void?>");
    }

    @Test
    public void generate_WithAbstractInheritance_PreservesNullablePropertiesAndDiscriminators() throws IOException {
        KotlinServerCodegen codegen = codegen();

        List<File> files = generate(codegen);

        assertThat(find(files, "Animal.kt")).content()
                .contains("abstract class Animal", "open val kind: AnimalKind? = null",
                        "open val nickname: kotlin.String?",
                        "JsonTypeInfo.As.EXISTING_PROPERTY", "visible = true");
        assertThat(find(files, "Cat.kt")).content()
                .contains("override val kind: AnimalKind? = null", "override val nickname: kotlin.String?",
                        "@JsonProperty(value = \"nickname\", required = true)")
                .doesNotContain("typealias");
        assertThat(find(files, "Choice.kt")).content()
                .contains("abstract class Choice", "JsonSubTypes.Type(value = FirstChoice::class, name = \"first\")")
                .doesNotContain("typealias");
    }

    @Test
    public void deserialize_WithDiscriminator_RoundTripsInheritanceAndUnions() throws Exception {
        KotlinServerCodegen codegen = codegen();
        codegen.additionalProperties().put("useJakartaEe", false);
        List<File> files = generate(codegen);
        ClassLoader compiled = compileModels(files);
        ObjectMapper mapper = new ObjectMapper();
        String[][] fixtures = {
                {"Animal", "Cat", "{\"kind\":\"cat\",\"name\":\"Luna\",\"nickname\":null,\"labels\":[\"friendly\",null],\"lives\":9}"},
                {"Animal", "Dog", "{\"kind\":\"dog\",\"name\":\"Rex\",\"nickname\":null,\"labels\":[],\"breed\":\"collie\"}"},
                {"Choice", "FirstChoice", "{\"kind\":\"first\",\"text\":\"hello\"}"},
                {"Choice", "SecondChoice", "{\"kind\":\"second\",\"number\":2}"},
                {"Alternative", "TextAlternative", "{\"kind\":\"text\",\"text\":\"hello\"}"},
                {"Alternative", "NumberAlternative", "{\"kind\":\"number\",\"number\":2}"}
        };

        for (String[] fixture : fixtures) {
            Object value = mapper.readValue(fixture[2], compiled.loadClass("org.openapitools.server.models." + fixture[0]));
            String encoded = mapper.writeValueAsString(value);

            assertThat(value.getClass().getSimpleName()).isEqualTo(fixture[1]);
            assertThat(mapper.readTree(encoded)).isEqualTo(mapper.readTree(fixture[2]));
            assertThat(encoded.split("\"kind\"", -1)).hasSize(2);
        }
    }

    @Test
    public void deserialize_WithInvalidRequiredProperties_RejectsJson() throws Exception {
        KotlinServerCodegen codegen = codegen();
        codegen.additionalProperties().put("useJakartaEe", false);
        List<File> files = generate(codegen);
        ClassLoader compiled = compileModels(files);
        Class<?> animal = compiled.loadClass("org.openapitools.server.models.Animal");
        ObjectMapper mapper = new ObjectMapper();
        String missing = "{\"kind\":\"cat\",\"name\":\"Luna\",\"labels\":[],\"lives\":9}";
        String unknown = "{\"kind\":\"unknown\",\"name\":\"Luna\",\"nickname\":null,\"labels\":[]}";
        String nullPrimitive = "{\"kind\":\"cat\",\"name\":\"Luna\",\"nickname\":null,\"labels\":[],\"lives\":null}";

        org.assertj.core.api.ThrowableAssert.ThrowingCallable readMissing = () -> mapper.readValue(missing, animal);
        org.assertj.core.api.ThrowableAssert.ThrowingCallable readUnknown = () -> mapper.readValue(unknown, animal);
        org.assertj.core.api.ThrowableAssert.ThrowingCallable readNullPrimitive = () -> mapper.readValue(nullPrimitive, animal);

        assertThatThrownBy(readMissing).isInstanceOf(MismatchedInputException.class).hasMessageContaining("nickname");
        assertThatThrownBy(readUnknown).isInstanceOf(InvalidTypeIdException.class);
        assertThatThrownBy(readNullPrimitive).isInstanceOf(MismatchedInputException.class).hasMessageContaining("lives");
    }

    private KotlinServerCodegen codegen() {
        KotlinServerCodegen codegen = new KotlinServerCodegen();
        codegen.setLibrary("jaxrs-spec");
        codegen.additionalProperties().put("interfaceOnly", true);
        codegen.additionalProperties().put("useJakartaEe", true);
        codegen.additionalProperties().put("useMutiny", true);
        codegen.additionalProperties().put("useBeanValidation", true);
        codegen.additionalProperties().put("omitGradleWrapper", true);
        return codegen;
    }

    private List<File> generate(KotlinServerCodegen codegen) throws IOException {
        codegen.setOutputDir(Files.createTempDirectory("kotlin-jaxrs-polymorphism").toString());
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);

        return generator.opts(new ClientOptInput().config(codegen).openAPI(TestUtils.parseSpec(SPEC))).generate();
    }

    private Path find(List<File> files, String filename) {
        return files.stream().filter(file -> file.getName().equals(filename))
                .findFirst().orElseThrow().toPath();
    }

    private ClassLoader compileModels(List<File> files) {
        List<String> sources = files.stream()
                .filter(file -> file.getParentFile().getName().equals("models") && file.getName().endsWith(".kt"))
                .map(File::getAbsolutePath).collect(Collectors.toList());
        return KotlinTestUtils.buildModule(sources, getClass().getClassLoader());
    }
}
