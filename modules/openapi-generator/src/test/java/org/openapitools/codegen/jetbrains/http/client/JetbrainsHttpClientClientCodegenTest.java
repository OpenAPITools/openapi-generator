package org.openapitools.codegen.jetbrains.http.client;

import org.junit.Ignore;
import org.junit.Test;
import org.openapitools.codegen.*;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.JetbrainsHttpClientClientCodegen;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.openapitools.codegen.TestUtils.assertFileExists;

public class JetbrainsHttpClientClientCodegenTest {

    @Test
    public void testBasicGenerationYaml() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/Basic.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/BasicApi.http");
        assertFileExists(path);
        TestUtils.assertFileContains(path, "## BasicApi\n" +
                "\n" +
                "### Get User\n" +
                "## Get User\n" +
                "GET http://localhost:5001/users/{{userId}}\n" +
                "Accept: application/json\n" +
                "Accept: application/xml");
    }

    @Test
    public void testBasicGenerationJson() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/BasicJson.json")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/BasicApi.http");
        assertFileExists(path);
        TestUtils.assertFileContains(path, "## BasicApi\n" +
                "\n" +
                "### Get User\n" +
                "## Get User\n" +
                "GET http://localhost:5000/v1/users/{{userId}}\n" +
                "Accept: application/json\n" +
                "Accept: application/xml");
    }

    @Test
    public void testBasicGenerationVariables() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/BasicVariablesInExample.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/BasicApi.http");
        assertFileExists(path);
        TestUtils.assertFileContains(path, "## BasicApi\n" +
                "\n" +
                "### Patch User\n" +
                "## Example patch user\n" +
                "PATCH http://localhost:5001/users/{{userId}}\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "Accept: application/xml\n" +
                "\n" +
                "{\n" +
                " \"id\": 1,\n" +
                " \"firstName\": \"MY_VAR_NAME\",\n" +
                " \"lastName\": \"MY_VAR_LAST_NAME\",\n" +
                " \"email\": \"alotta.rotta@gmail.com\",\n" +
                " \"dateOfBirth\": \"1997-10-31\",\n" +
                " \"emailVerified\": true,\n" +
                " \"createDate\": \"RANDOM_VALUE\"\n" +
                "}");
    }

    @Test
    public void testBasicGenerationVariablesWithBodyVariables() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/BasicVariablesInExample.yaml")
                .addAdditionalProperty(JetbrainsHttpClientClientCodegen.BODY_VARIABLES, "MY_VAR_NAME-MY_VAR_LAST_NAME")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/BasicApi.http");
        assertFileExists(path);
        TestUtils.assertFileContains(path, "## BasicApi\n" +
                "\n" +
                "### Patch User\n" +
                "## Example patch user\n" +
                "PATCH http://localhost:5001/users/{{userId}}\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "Accept: application/xml\n" +
                "\n" +
                "{\n" +
                " \"id\": 1,\n" +
                " \"firstName\": \"{{MY_VAR_NAME}}\",\n" +
                " \"lastName\": \"{{MY_VAR_LAST_NAME}}\",\n" +
                " \"email\": \"alotta.rotta@gmail.com\",\n" +
                " \"dateOfBirth\": \"1997-10-31\",\n" +
                " \"emailVerified\": true,\n" +
                " \"createDate\": \"RANDOM_VALUE\"\n" +
                "}");
    }

    @Test
    public void testBasicGenerationWithCustomHeaders() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasic.yaml")
                .addAdditionalProperty(JetbrainsHttpClientClientCodegen.CUSTOM_HEADERS, "Cookie:X-API-KEY={{cookieKey}}&Accept-Encoding=gzip")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);
        TestUtils.assertFileContains(path, "### Make a payment\n" +
                "## GooglePay request\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "Cookie:X-API-KEY={{cookieKey}}\n" +
                "Accept-Encoding=gzip");
    }

    @Test
    public void testBasicGenerationAuthBearer() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicBearer.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        // Checking first and last
        TestUtils.assertFileContains(path, "## PaymentsApi\n" +
                "\n" +
                "### Get payment method by id\n" +
                "## Get payment method by id\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods/{{id}}\n" +
                "Accept: application/json\n" +
                "Authorization: Bearer {{bearerToken}}");

        TestUtils.assertFileContains(path, "### Make a payment\n" +
                "## Example with a merchant account that doesn&#39;t exist\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "Authorization: Bearer {{bearerToken}}");
    }

    @Test
    public void testBasicGenerationAuthCookie() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicCookie.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        // Checking first and last
        TestUtils.assertFileContains(path, "## PaymentsApi\n" +
                "\n" +
                "### Get payment method by id\n" +
                "## Get payment method by id\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods/{{id}}\n" +
                "Accept: application/json\n" +
                "Cookie: X-API-Key={{cookieKey}}");

        TestUtils.assertFileContains(path, "### Make a payment\n" +
                "## Example with a merchant account that doesn&#39;t exist\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "Cookie: X-API-Key={{cookieKey}}");
    }

    @Test
    public void testBasicGenerationAuthQuery() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicQuery.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        // Checking first and last
        TestUtils.assertFileContains(path, "## PaymentsApi\n" +
                "\n" +
                "### Get payment method by id\n" +
                "## Get payment method by id\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods/{{id}}?api_key={{queryKey}}");

        TestUtils.assertFileContains(path, "### Make a payment\n" +
                "## Example with a merchant account that doesn&#39;t exist\n" +
                "POST https://checkout-test.adyen.com/v71/payments?api_key={{queryKey}}\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "\n" +
                "{\n" +
                "  \"paymentMethod\" : {\n" +
                "    \"name\" : \"googlepay\"\n" +
                "  },\n" +
                "  \"amount\" : {\n" +
                "    \"currency\" : \"EUR\",\n" +
                "    \"value\" : 1000\n" +
                "  },\n" +
                "  \"merchantAccount\" : \"INVALID MERCHANT ACCOUNT\",\n" +
                "  \"reference\" : \"YOUR_REFERENCE\",\n" +
                "  \"channel\" : \"Android\"\n" +
                "}");
    }

    @Test
    public void testBasicGenerationAuthBasic() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicBasic.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        TestUtils.assertFileContains(path, "## PaymentsApi\n" +
                "\n" +
                "### Get payment method by id\n" +
                "## Get payment method by id\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods/{{id}}\n" +
                "Accept: application/json\n" +
                "Authorization: Basic: {{username-password}}");
    }

    @Test
    public void testBasicGenerationAuthHeader() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicHeader.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        TestUtils.assertFileContains(path, "## PaymentsApi\n" +
                "\n" +
                "### Get payment method by id\n" +
                "## Get payment method by id\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods/{{id}}\n" +
                "Accept: application/json\n" +
                "X-API-Key: {{apiKey}}");
    }

    @Test
    public void testBasicGenerationManyAuths() throws IOException {

        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicBearerCookieQueryHeaderBasicBearer.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        TestUtils.assertFileContains(path, "### Get payment method by id\n" +
                "## Get payment method by id\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods/{{id}}?api_key={{queryKey}}\n" +
                "Accept: application/json\n" +
                "Authorization: Bearer {{bearerToken}}");

        TestUtils.assertFileContains(path, "### Get payment methods\n" +
                "## Get payment methods\n" +
                "GET https://checkout-test.adyen.com/v71/paymentMethods\n" +
                "Accept: application/json\n" +
                "Authorization: Basic: {{username-password}}\n" +
                "Authorization: Bearer {{bearerToken}}");

        TestUtils.assertFileContains(path, "### Make a payment\n" +
                "## Example with a merchant account that doesn&#39;t exist\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "Cookie: X-API-Key={{cookieKey}}\n" +
                "Authorization: Bearer {{bearerToken}}");
    }

    @Test
    @Ignore // For some reason this test fails during Docker image generation. Investigate one day.
    public void testBasicGenerationMultipleRequests() throws IOException {
        // Checking that each request example is present in the output file
        File output = Files.createTempDirectory("jetbrainstest_").toFile();
        output.deleteOnExit();

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("jetbrains-http-client")
                .setInputSpec("src/test/resources/3_0/jetbrains/CheckoutBasicMultiplekeys.yaml")
                .setOutputDir(output.getAbsolutePath().replace("\\", "/"));

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        DefaultGenerator generator = new DefaultGenerator();
        List<File> files = generator.opts(clientOptInput).generate();

        files.forEach(File::deleteOnExit);

        Path path = Paths.get(output + "/Apis/PaymentsApi.http");
        assertFileExists(path);

        // Checking first and last
        TestUtils.assertFileContains(path, "### Make a payment\n" +
                "## ApplePay request\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "\n" +
                "{\n" +
                "  \"paymentMethod\" : {\n" +
                "    \"name\" : \"applepay\"\n" +
                "  },\n" +
                "  \"amount\" : {\n" +
                "    \"currency\" : \"EUR\",\n" +
                "    \"value\" : 1000\n" +
                "  },\n" +
                "  \"merchantAccount\" : \"YOUR_MERCHANT_ACCOUNT\",\n" +
                "  \"reference\" : \"YOUR_REFERENCE\",\n" +
                "  \"channel\" : \"iOS\"\n" +
                "}\n" +
                "\n" +
                "### Make a payment\n" +
                "## GooglePay request\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "\n" +
                "{\n" +
                "  \"paymentMethod\" : {\n" +
                "    \"name\" : \"googlepay\"\n" +
                "  },\n" +
                "  \"amount\" : {\n" +
                "    \"currency\" : \"EUR\",\n" +
                "    \"value\" : 1000\n" +
                "  },\n" +
                "  \"merchantAccount\" : \"YOUR_MERCHANT_ACCOUNT\",\n" +
                "  \"reference\" : \"YOUR_REFERENCE\",\n" +
                "  \"channel\" : \"Android\"\n" +
                "}\n" +
                "\n" +
                "### Make a payment\n" +
                "## Example with a merchant account that doesn&#39;t exist\n" +
                "POST https://checkout-test.adyen.com/v71/payments\n" +
                "Content-Type: application/json\n" +
                "Accept: application/json\n" +
                "\n" +
                "{\n" +
                "  \"paymentMethod\" : {\n" +
                "    \"name\" : \"googlepay\"\n" +
                "  },\n" +
                "  \"amount\" : {\n" +
                "    \"currency\" : \"EUR\",\n" +
                "    \"value\" : 1000\n" +
                "  },\n" +
                "  \"merchantAccount\" : \"INVALID MERCHANT ACCOUNT\",\n" +
                "  \"reference\" : \"YOUR_REFERENCE\",\n" +
                "  \"channel\" : \"Android\"\n" +
                "}");
    }
}
