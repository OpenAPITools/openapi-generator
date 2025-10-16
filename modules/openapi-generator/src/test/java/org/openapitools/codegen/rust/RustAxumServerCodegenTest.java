package org.openapitools.codegen.rust;

import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.TestUtils;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.openapitools.codegen.TestUtils.linearize;

public class RustAxumServerCodegenTest {
    @Test
    public void testPreventDuplicateOperationDeclaration() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/issue_21144.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        Path outputPath = Path.of(target.toString(), "/src/server/mod.rs");
        String routerSpec = linearize("Router::new() " +
                ".route(\"/api/test\", " +
                "delete(test_delete::<I, A, E, C>).post(test_post::<I, A, E, C>) ) " +
                ".route(\"/api/test/{test_id}\", " +
                "get(test_get::<I, A, E, C>) ) " +
                ".with_state(api_impl)");
        TestUtils.assertFileExists(outputPath);
        TestUtils.assertFileContains(outputPath, routerSpec);
    }

    @Test
    public void testMultipleContentTypesPerStatusCode() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/rust-axum/test-multiple-content-types.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        
        Path apiPath = Path.of(target.toString(), "/src/apis/default.rs");
        TestUtils.assertFileExists(apiPath);
        
        TestUtils.assertFileContains(apiPath, "pub enum TestGetResponse");
        
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithMultipleContentTypes_Json");
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithMultipleContentTypes_EventStream");
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithMultipleContentTypes_PlainText");
        
        TestUtils.assertFileContains(apiPath, "(application/json)");
        TestUtils.assertFileContains(apiPath, "(text/event-stream)");
        TestUtils.assertFileContains(apiPath, "(text/plain)");
        
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithMultipleContentTypes_Json");
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithMultipleContentTypes_EventStream");
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithMultipleContentTypes_PlainText");
        
        TestUtils.assertFileContains(apiPath, "(models::TestGet200Response)");
        TestUtils.assertFileContains(apiPath, "(std::pin::Pin<Box<dyn futures::Stream<Item = Result<String, Box<dyn std::error::Error + Send + Sync + 'static>>> + Send + 'static>>)");
        TestUtils.assertFileContains(apiPath, "(String)");
        
        TestUtils.assertFileContains(apiPath, "Status400_BadRequest_Json");
    }

    @Test
    public void testComplexEventStreamType() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/rust-axum/test-complex-event-stream.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));
        
        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);
        
        Path apiPath = Path.of(target.toString(), "/src/apis/default.rs");
        TestUtils.assertFileExists(apiPath);
        
        TestUtils.assertFileContains(apiPath, "pub enum EventsGetResponse");
        TestUtils.assertFileContains(apiPath, "Status200_SuccessResponseWithComplexEventStream_EventStream");
        TestUtils.assertFileContains(apiPath, "(text/event-stream)");
        TestUtils.assertFileContains(apiPath, "(std::pin::Pin<Box<dyn futures::Stream<Item = Result<models::EventsGet200Response, Box<dyn std::error::Error + Send + Sync + 'static>>> + Send + 'static>>)");
        TestUtils.assertFileContains(apiPath, "Status400_BadRequest_Json");
    }

    @Test
    public void testOneOfResponse() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/rust-axum/test-oneof-response.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiPath = Path.of(target.toString(), "/src/apis/default.rs");
        TestUtils.assertFileExists(apiPath);

        TestUtils.assertFileContains(apiPath, "pub enum PetsGetResponse");

        TestUtils.assertFileContains(apiPath, "Status200_APetObject_Json");
        TestUtils.assertFileContains(apiPath, "Status200_APetObject_EventStream");
        TestUtils.assertFileContains(apiPath, "(application/json)");
        TestUtils.assertFileContains(apiPath, "(text/event-stream)");
        TestUtils.assertFileContains(apiPath, "(models::PetsGet200Response)");
        TestUtils.assertFileContains(apiPath, "(std::pin::Pin<Box<dyn futures::Stream<Item = Result<models::PetsGet200Response, Box<dyn std::error::Error + Send + Sync + 'static>>> + Send + 'static>>)");
        TestUtils.assertFileContains(apiPath, "Status400_BadRequest_Json");
    }

    @Test
    public void testAnyOfAllOfResponse() throws IOException {
        Path target = Files.createTempDirectory("test");
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName("rust-axum")
                .setInputSpec("src/test/resources/3_1/rust-axum/test-anyof-allof-response.yaml")
                .setSkipOverwrite(false)
                .setOutputDir(target.toAbsolutePath().toString().replace("\\", "/"));

        List<File> files = new DefaultGenerator().opts(configurator.toClientOptInput()).generate();
        files.forEach(File::deleteOnExit);

        Path apiPath = Path.of(target.toString(), "/src/apis/default.rs");
        TestUtils.assertFileExists(apiPath);

        TestUtils.assertFileContains(apiPath, "pub enum AnimalsGetResponse");
        TestUtils.assertFileContains(apiPath, "pub enum HybridGetResponse");

        TestUtils.assertFileContains(apiPath, "Status200_AnAnimalObject_Json");
        TestUtils.assertFileContains(apiPath, "Status200_AnAnimalObject_EventStream");
        
        TestUtils.assertFileContains(apiPath, "Status200_AHybridAnimal_Json");
        TestUtils.assertFileContains(apiPath, "Status200_AHybridAnimal_EventStream");

        TestUtils.assertFileContains(apiPath, "(application/json)");
        TestUtils.assertFileContains(apiPath, "(text/event-stream)");

        TestUtils.assertFileContains(apiPath, "(models::AnimalsGet200Response)");
        TestUtils.assertFileContains(apiPath, "(models::HybridGet200Response)");
        TestUtils.assertFileContains(apiPath, "(std::pin::Pin<Box<dyn futures::Stream<Item = Result<models::AnimalsGet200Response, Box<dyn std::error::Error + Send + Sync + 'static>>> + Send + 'static>>)");
        TestUtils.assertFileContains(apiPath, "(std::pin::Pin<Box<dyn futures::Stream<Item = Result<models::HybridGet200Response, Box<dyn std::error::Error + Send + Sync + 'static>>> + Send + 'static>>)");

        TestUtils.assertFileContains(apiPath, "Status400_BadRequest_Json");
    }
}