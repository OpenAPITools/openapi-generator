package org.openapitools.codegen.php.laravel;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PhpLaravelServerCodegenTest {

    /**
     * Parameters whose spec (wire) name is not already a valid camelCase PHP identifier must be read
     * from the request and validated under the wire name ({@code baseName}), not under the sanitized
     * PHP variable name ({@code paramName}). Otherwise the generated controller reads the wrong key
     * and the value is always null.
     */
    @Test
    public void shouldUseWireNameForRequestLookupsAndValidationKeys() throws Exception {
        Map<String, Object> properties = new HashMap<>();

        File output = Files.createTempDirectory("test").toFile();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("php-laravel")
                .setAdditionalProperties(properties)
                .setInputSpec("src/test/resources/3_0/php-laravel/petstore-with-fake-endpoints-models-for-testing.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        TestUtils.ensureContainsFile(files, output, "Http/Controllers/FakeController.php");
        TestUtils.ensureContainsFile(files, output, "Http/Controllers/StoreController.php");
        TestUtils.ensureContainsFile(files, output, "Http/Controllers/UserController.php");

        Path fakeController = files.stream().filter(f -> f.getName().equals("FakeController.php")).findFirst().orElseThrow().toPath();

        // request accessors use the wire name (snake_case) while the local variable keeps the sanitized name
        TestUtils.assertFileContains(fakeController, "$filterClientName = $request->string('filter_client_name')->value();");
        TestUtils.assertFileContains(fakeController, "$filterUserIds = $request->get('filter_user_ids');");
        TestUtils.assertFileContains(fakeController, "$clientId = $request->integer('client_id');");

        // validation rule keys must match $request->all(), i.e. the wire name
        TestUtils.assertFileContains(fakeController, "'filter_client_name' => [");
        TestUtils.assertFileContains(fakeController, "'filter_user_ids' => [");
        TestUtils.assertFileContains(fakeController, "'client_id' => [");

        // the buggy behaviour (reading under the sanitized identifier) must not reappear
        TestUtils.assertFileNotContains(fakeController, "$request->string('filterClientName')");
        TestUtils.assertFileNotContains(fakeController, "$request->get('filterUserIds')");
        TestUtils.assertFileNotContains(fakeController, "$request->integer('clientId')");

        // path parameters: the merged validation key and the rule key both use the wire name
        Path storeController = files.stream().filter(f -> f.getName().equals("StoreController.php")).findFirst().orElseThrow().toPath();
        TestUtils.assertFileContains(storeController, "'order_id' => $orderId,");
        TestUtils.assertFileContains(storeController, "'order_id' => [");

        // body parameters are out of scope and must stay byte-identical: no wire-name substitution for the request body
        Path userController = files.stream().filter(f -> f.getName().equals("UserController.php")).findFirst().orElseThrow().toPath();
        TestUtils.assertFileContains(userController, "$user = $request->get('user');");

        output.deleteOnExit();
    }
}
