package org.openapitools.codegen.php.flight;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PhpFlightServerCodegenTest {

    @Test
    public void shouldGenerateModel() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-flight")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/petstore-php-flight.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "Api/AbstractPetApi.php");
        TestUtils.ensureContainsFile(files, output, "Model/Pet.php");
        TestUtils.ensureContainsFile(files, output, "Model/StandaloneEnum.php");
        TestUtils.ensureContainsFile(files, output, "Model/PetStatus.php"); // inline enum
        TestUtils.ensureContainsFile(files, output, "README.md");
        TestUtils.ensureContainsFile(files, output, "RegisterRoutes.php");

        java.nio.file.Path petModelFile = files.stream().filter(f -> f.getName().contains("Pet.php")).findFirst().orElseThrow().toPath();
        TestUtils.assertFileContains(petModelFile, "namespace OpenAPIServer\\Model;");
        TestUtils.assertFileContains(petModelFile, "public int $id;");
        TestUtils.assertFileContains(petModelFile, "public ?string $name;");
        TestUtils.assertFileContains(petModelFile, "public ?\\DateTime $dateTimeAttribute;");
        TestUtils.assertFileContains(petModelFile, "@var Order[]|null");
        TestUtils.assertFileContains(petModelFile, "public ?array $objectList;");
        TestUtils.assertFileContains(petModelFile, "$data['photo_urls'] ?? null,");
        TestUtils.assertFileContains(petModelFile, "'photo_urls' => $this->photoUrls");

        TestUtils.assertFileContains(petModelFile, "isset($data['category']) ? Category::fromArray($data['category']) : null,");
        TestUtils.assertFileContains(petModelFile, "isset($data['status']) ? PetStatus::tryFrom($data['status']) : null");
        TestUtils.assertFileContains(petModelFile, "isset($data['refEnum']) ? StandaloneEnum::tryFrom($data['refEnum']) : null");
        TestUtils.assertFileContains(petModelFile, "isset($data['dateTimeAttribute']) ? new \\DateTime($data['dateTimeAttribute']) : null");

        java.nio.file.Path petApiFile = files.stream().filter(f -> f.getName().contains("AbstractPetApi.php")).findFirst().orElseThrow().toPath();
        TestUtils.assertFileContains(petApiFile, "namespace OpenAPIServer\\Api;");
        TestUtils.assertFileContains(petApiFile, "public function getPetById(int $petId)");
        TestUtils.assertFileContains(petApiFile, "public function updatePet(\\OpenAPIServer\\Model\\Pet $pet): \\OpenAPIServer\\Model\\Pet|null");

        java.nio.file.Path registerRoutesFile = files.stream().filter(f -> f.getName().contains("RegisterRoutes.php")).findFirst().orElseThrow().toPath();
        TestUtils.assertFileContains(registerRoutesFile, "function registerRoutes(\\OpenAPIServer\\Api\\AbstractPetApi|\\OpenAPIServer\\Api\\AbstractUserApi $handler): void");
        TestUtils.assertFileContains(registerRoutesFile,
                "Flight::route('POST /user/createWithArray/@pathParamInt/@pathParamString', function (string $pathParamInt, string $pathParamString) use ($handler) {");
        TestUtils.assertFileContains(registerRoutesFile, "parseParam($pathParamInt, 'int')");
        TestUtils.assertFileContains(registerRoutesFile, "parseParam($pathParamString, 'string')");
        TestUtils.assertFileContains(registerRoutesFile, "parseParam(json_decode($r->getBody(), true), '\\\\OpenAPIServer\\\\Model\\\\User[]')");
        TestUtils.assertFileContains(registerRoutesFile, "parseParam($r->getHeader('api_key'), '?string')");

        Files.readAllLines(files.stream().filter(f -> f.getName().contains("RegisterRoutesTest.php")).findFirst().orElseThrow().toPath()).forEach(System.out::println);
        java.nio.file.Path registerRoutesTestFile = files.stream().filter(f -> f.getName().contains("RegisterRoutesTest.php")).findFirst().orElseThrow().toPath();
        TestUtils.assertFileContains(registerRoutesTestFile, "namespace OpenAPIServer\\Test;");

        output.deleteOnExit();
    }
}
