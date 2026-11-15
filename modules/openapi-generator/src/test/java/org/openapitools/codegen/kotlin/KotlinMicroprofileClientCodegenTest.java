package org.openapitools.codegen.kotlin;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Content;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.ObjectSchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.parser.OpenAPIV3Parser;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.languages.KotlinClientCodegen;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static kotlin.script.experimental.jvm.util.JvmClasspathUtilKt.classpathFromClassloader;

public class KotlinMicroprofileClientCodegenTest {
    private static final String SPEC = "src/test/resources/3_0/kotlin/microprofile.yaml";

    @DataProvider
    public Object[][] runtimes() {
        return new Object[][]{{"3.0", false, "jakarta"}, {"3.0", true, "jakarta"},
                {"2.0", false, "javax"}, {"2.0", true, "javax"}};
    }

    @Test(dataProvider = "runtimes")
    public void generate_WithSupportedRuntime_CompilesKotlin(String version, boolean mutiny, String namespace) throws Exception {
        KotlinClientCodegen codegen = codegen();
        codegen.additionalProperties().put(KotlinClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, version);
        codegen.additionalProperties().put(KotlinClientCodegen.MICROPROFILE_MUTINY, mutiny);
        codegen.additionalProperties().put(KotlinClientCodegen.CONFIG_KEY, "pet-service");

        List<File> files = generate(codegen);
        Path api = find(files, "PetApi.kt");
        Path pet = find(files, "Pet.kt");
        List<String> sources = files.stream().filter(file -> file.toPath().startsWith(Path.of(codegen.getOutputDir(), "src", "main", "kotlin"))
                        && file.getName().endsWith(".kt"))
                .map(File::getAbsolutePath).collect(Collectors.toList());
        assertThat(sources).isNotEmpty();
        ClassLoader compiled;
        try (URLClassLoader runtime = runtimeClasspath(version)) {
            compiled = KotlinTestUtils.buildModule(sources, runtime);

            assertThat(compiled.loadClass("org.openapitools.client.apis.PetApi").isInterface()).isTrue();
            assertThat(compiled.loadClass("org.eclipse.microprofile.rest.client.inject.RegisterRestClient")
                    .getProtectionDomain().getCodeSource().getLocation().getPath())
                    .endsWith("microprofile-rest-client-api-" + version + ".jar");
        }
        assertThat(api).content().contains("import " + namespace + ".ws.rs.*",
                "@RegisterRestClient(configKey = \"pet-service\")", "@Path(\"/pets/{petId}\")",
                "@PathParam(\"petId\") petId: kotlin.Long",
                "@QueryParam(\"filter\") filter: kotlin.String? = null",
                "@HeaderParam(\"X-Request-Id\") xRequestId: kotlin.String? = null",
                "@CookieParam(\"session\") session: kotlin.String? = null",
                "@QueryParam(\"value\") `value`: kotlin.String?",
                "pet: Pet");
        assertThat(api).content().doesNotContain("`value`: kotlin.String? = null");
        assertThat(api).content().contains(mutiny ? "): Uni<Pet>" : "): Pet",
                mutiny ? "): Uni<Void?>" : "): Unit",
                mutiny ? "): Uni<kotlin.String?>" : "): kotlin.String?");
        assertThat(pet).content().contains("val name: kotlin.String", "val nickname: kotlin.String?",
                "val labels: kotlin.collections.List<kotlin.String?>", "val age: kotlin.Int? = null",
                "@param:JsonProperty(value = \"nickname\", required = true)", "@param:JsonProperty(\"age\")");
        assertThat(compiled.loadClass("org.openapitools.client.models.Choice"))
                .isAssignableFrom(compiled.loadClass("org.openapitools.client.models.FirstChoice"));
        assertThat(compiled.loadClass("org.openapitools.client.models.Alternative"))
                .isAssignableFrom(compiled.loadClass("org.openapitools.client.models.FirstChoice"));
        assertThat(find(files, "Choice.kt")).content().doesNotContain("val text", "val number");
        assertThat(find(files, "build.gradle")).content()
                .contains("microprofile-rest-client-api:" + version, "apply plugin: 'java-library'", "-Xjvm-default=all")
                .doesNotContain("okhttp", "moshi", "retrofit", "ApiClient.kt");
        assertThat(find(files, "beans.xml")).content()
                .contains("bean-discovery-mode=\"annotated\"", "version=\"" + version + "\"",
                        namespace.equals("jakarta") ? "https://jakarta.ee/xml/ns/jakartaee" : "http://xmlns.jcp.org/xml/ns/javaee");
        assertThat(files).noneMatch(file -> file.getName().equals("ApiClient.kt") || file.getName().endsWith("ApiTest.kt"));
        assertThat(find(files, "PetApi.md")).content()
                .contains("`` filter ``: `kotlin.String? = null`", "`` `value` ``: `kotlin.String?`",
                        mutiny ? "Returns `Uni<kotlin.String?>`" : "Returns `kotlin.String?`",
                        mutiny ? "Returns `Uni<Void?>`" : "Returns `Unit`");
        assertThat(find(files, "Pet.md")).content().contains("**kotlin.String?**", "**kotlin.Int?**",
                "**java.time.LocalDate?**").doesNotContain("String??", "Int??");
        assertThat(find(files, "Cat.md")).content().contains("Inherits properties from [Animal](Animal.md)");
    }

    @Test
    public void generate_WithNonMicroprofileJacksonLibrary_PreservesRequiredPropertyFormatting() throws IOException {
        KotlinClientCodegen codegen = codegen();
        codegen.setLibrary("jvm-retrofit2");
        codegen.additionalProperties().put(CodegenConstants.SERIALIZATION_LIBRARY, "jackson");

        List<File> files = generate(codegen);

        assertThat(find(files, "Pet.kt")).content()
                .contains("@param:JsonProperty(\"name\")\n    @get:JsonProperty(\"name\")\n    val name:")
                .doesNotContain("JsonSetter");
    }

    @Test
    public void generate_WithSerializableUnionModels_CompilesAllInterfaces() throws Exception {
        KotlinClientCodegen codegen = codegen();
        codegen.additionalProperties().put("serializableModel", true);

        List<File> files = generate(codegen);
        List<String> sources = files.stream().filter(file -> file.toPath().startsWith(Path.of(codegen.getOutputDir(), "src", "main", "kotlin"))
                        && file.getName().endsWith(".kt"))
                .map(File::getAbsolutePath).collect(Collectors.toList());
        assertThat(sources).isNotEmpty();
        ClassLoader compiled = KotlinTestUtils.buildModule(sources, getClass().getClassLoader());

        assertThat(compiled.loadClass("org.openapitools.client.models.Choice"))
                .isAssignableFrom(compiled.loadClass("org.openapitools.client.models.FirstChoice"));
        assertThat(compiled.loadClass("org.openapitools.client.models.Alternative"))
                .isAssignableFrom(compiled.loadClass("org.openapitools.client.models.FirstChoice"));
    }

    @Test
    public void generate_WithOpenApi31_PreservesNullableTypes() throws Exception {
        KotlinClientCodegen codegen = codegen();

        List<File> files = generate(codegen, "src/test/resources/3_1/kotlin-microprofile-nullability.yaml");
        List<String> sources = files.stream().filter(file -> file.toPath().startsWith(Path.of(codegen.getOutputDir(), "src", "main", "kotlin"))
                        && file.getName().endsWith(".kt"))
                .map(File::getAbsolutePath).collect(Collectors.toList());
        assertThat(sources).isNotEmpty();
        ClassLoader compiled = KotlinTestUtils.buildModule(sources, getClass().getClassLoader());

        assertThat(compiled.loadClass("org.openapitools.client.apis.PetApi").isInterface()).isTrue();
        assertThat(find(files, "Pet.kt")).content().contains("val name: kotlin.String", "val nickname: kotlin.String?",
                "val labels: kotlin.collections.List<kotlin.String?>");
        assertThat(find(files, "PetApi.kt")).content().contains("@QueryParam(\"filter\") filter: kotlin.String? = null");
    }

    @Test
    public void processOpts_WithDefaults_SelectsJakartaAndJackson() {
        KotlinClientCodegen codegen = codegen();

        codegen.processOpts();

        assertThat(codegen.getSerializationLibrary()).isEqualTo(KotlinClientCodegen.SERIALIZATION_LIBRARY_TYPE.jackson);
        assertThat(codegen.additionalProperties()).containsEntry("rootJavaEEPackage", "jakarta")
                .containsEntry(KotlinClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "3.0")
                .containsEntry(KotlinClientCodegen.MICROPROFILE_MUTINY, false);
    }

    @Test
    public void generate_WithClassNameConfigKey_DerivesOneKey() throws IOException {
        KotlinClientCodegen codegen = codegen();
        codegen.additionalProperties().put(KotlinClientCodegen.CONFIG_KEY_FROM_CLASS_NAME, "true");

        List<File> files = generate(codegen);

        assertThat(find(files, "PetApi.kt")).content().contains("@RegisterRestClient(configKey = \"pet-api\")");
    }

    @DataProvider
    public Object[][] invalidOptions() {
        return new Object[][]{
                {Map.of(CodegenConstants.SERIALIZATION_LIBRARY, "gson"), "requires serializationLibrary=jackson"},
                {Map.of(KotlinClientCodegen.MICROPROFILE_REST_CLIENT_VERSION, "4.0"), "2.0 or 3.0"},
                {Map.of("useJakartaEe", false), "useJakartaEe must match"},
                {Map.of(KotlinClientCodegen.USE_COROUTINES, true), "not useCoroutines"},
                {Map.of(KotlinClientCodegen.USE_RX_JAVA3, true), "not useCoroutines"},
                {Map.of(KotlinClientCodegen.GENERATE_ROOM_MODELS, true), "does not support generateRoomModels"},
                {Map.of(KotlinClientCodegen.CONFIG_KEY, "pet", KotlinClientCodegen.CONFIG_KEY_FROM_CLASS_NAME, true), "not both"}
        };
    }

    @Test(dataProvider = "invalidOptions")
    public void processOpts_WithIncompatibleOptions_RejectsConfiguration(Map<String, Object> options, String message) {
        KotlinClientCodegen codegen = codegen();
        codegen.additionalProperties().putAll(options);

        Runnable process = codegen::processOpts;

        assertThatThrownBy(process::run).isInstanceOf(IllegalArgumentException.class).hasMessageContaining(message);
    }

    @DataProvider
    public Object[][] multipartMediaTypes() {
        return new Object[][]{{"multipart/form-data"}, {"Multipart/Form-Data"}, {"multipart/form-data; boundary=example"},
                {" MULTIPART/FORM-DATA ; boundary=example"}, {"multipart/mixed"}};
    }

    @Test(dataProvider = "multipartMediaTypes")
    public void generate_WithMultipartOperation_RejectsUnsupportedTransport(String mediaType) throws IOException {
        KotlinClientCodegen codegen = codegen();
        codegen.setOutputDir(Files.createTempDirectory("kotlin-microprofile-multipart").toString());
        OpenAPI spec = new OpenAPIV3Parser().read(SPEC);
        spec.getPaths().get("/pets").getPost().getRequestBody().getContent()
                .addMediaType(mediaType, spec.getPaths().get("/pets").getPost().getRequestBody().getContent().get("application/json"));
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(new ClientOptInput().config(codegen).openAPI(spec));

        Runnable generate = generator::generate;

        assertThatThrownBy(generate::run).hasStackTraceContaining("does not support multipart operations: createPet");
    }

    @DataProvider
    public Object[][] unsupportedQueryParameters() {
        return new Object[][]{
                {Parameter.StyleEnum.FORM, false, new ArraySchema().items(new StringSchema())},
                {Parameter.StyleEnum.SPACEDELIMITED, false, new ArraySchema().items(new StringSchema())},
                {Parameter.StyleEnum.PIPEDELIMITED, false, new ArraySchema().items(new StringSchema())},
                {Parameter.StyleEnum.DEEPOBJECT, true, new ObjectSchema().addProperty("name", new StringSchema())},
                {Parameter.StyleEnum.FORM, true, new ObjectSchema().addProperty("name", new StringSchema())},
                {Parameter.StyleEnum.FORM, false, new ObjectSchema()},
                {Parameter.StyleEnum.FORM, true, new ArraySchema().items(new ObjectSchema().addProperty("name", new StringSchema()))}
        };
    }

    @Test(dataProvider = "unsupportedQueryParameters")
    public void generate_WithUnsupportedQuerySerialization_RejectsOperation(Parameter.StyleEnum style, boolean explode, Schema<?> schema) throws IOException {
        KotlinClientCodegen codegen = codegen();
        codegen.setOutputDir(Files.createTempDirectory("kotlin-microprofile-query").toString());
        OpenAPI spec = new OpenAPIV3Parser().read(SPEC);
        Parameter parameter = spec.getPaths().get("/pets").getGet().getParameters().get(0);
        parameter.setStyle(style);
        parameter.setExplode(explode);
        parameter.setSchema(schema);
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(new ClientOptInput().config(codegen).openAPI(spec));

        Runnable generate = generator::generate;

        assertThatThrownBy(generate::run).hasStackTraceContaining("supports only scalar query parameters")
                .hasStackTraceContaining("listPets.tags");
    }

    @Test
    public void generate_WithNullableReturnOption_DocumentsSingleNullableMarker() throws IOException {
        KotlinClientCodegen codegen = codegen();
        codegen.additionalProperties().put("nullableReturnType", true);

        List<File> files = generate(codegen);

        assertThat(find(files, "PetApi.md")).content().contains("Returns `kotlin.String?`")
                .doesNotContain("String??");
    }

    @Test
    public void generate_WithContentBasedQuery_RejectsOperation() throws IOException {
        KotlinClientCodegen codegen = codegen();
        codegen.setOutputDir(Files.createTempDirectory("kotlin-microprofile-query-content").toString());
        OpenAPI spec = new OpenAPIV3Parser().read(SPEC);
        Parameter parameter = spec.getPaths().get("/pets").getGet().getParameters().get(0);
        parameter.setSchema(null);
        parameter.setContent(new Content().addMediaType("application/json", new MediaType().schema(new StringSchema())));
        DefaultGenerator generator = new DefaultGenerator();
        generator.opts(new ClientOptInput().config(codegen).openAPI(spec));

        Runnable generate = generator::generate;

        assertThatThrownBy(generate::run).hasStackTraceContaining("supports only scalar query parameters")
                .hasStackTraceContaining("listPets.tags");
    }

    private URLClassLoader runtimeClasspath(String version) throws IOException {
        List<URL> urls = new ArrayList<>();
        for (File file : classpathFromClassloader(getClass().getClassLoader(), false)) {
            if (!file.getName().startsWith("microprofile-rest-client-api-")) {
                urls.add(file.toURI().toURL());
            }
        }
        File api = "2.0".equals(version)
                ? Path.of("target/test-libs/microprofile-rest-client-api-2.0.jar").toFile()
                : new File(org.eclipse.microprofile.rest.client.inject.RegisterRestClient.class
                        .getProtectionDomain().getCodeSource().getLocation().getPath());
        assertThat(api).isFile();
        urls.add(api.toURI().toURL());
        return new URLClassLoader(urls.toArray(new URL[0]), ClassLoader.getPlatformClassLoader());
    }

    private KotlinClientCodegen codegen() {
        KotlinClientCodegen codegen = new KotlinClientCodegen();
        codegen.setLibrary(KotlinClientCodegen.JVM_MICROPROFILE);
        codegen.additionalProperties().put(KotlinClientCodegen.OMIT_GRADLE_WRAPPER, true);
        return codegen;
    }

    private List<File> generate(KotlinClientCodegen codegen) throws IOException {
        return generate(codegen, SPEC);
    }

    private List<File> generate(KotlinClientCodegen codegen, String spec) throws IOException {
        codegen.setOutputDir(Files.createTempDirectory("kotlin-microprofile").toString());
        DefaultGenerator generator = new DefaultGenerator();
        generator.setGenerateMetadata(false);
        generator.setGeneratorPropertyDefault("modelTests", "false");
        return generator.opts(new ClientOptInput().config(codegen).openAPI(new OpenAPIV3Parser().read(spec))).generate();
    }

    private Path find(List<File> files, String name) {
        return files.stream().filter(file -> file.getName().equals(name)).findFirst().orElseThrow().toPath();
    }
}
