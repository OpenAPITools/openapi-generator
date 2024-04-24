package org.openapitools.codegen.cmd;

import io.airlift.airline.Cli;
import org.testng.Assert;
import org.testng.ITestContext;
import org.testng.annotations.BeforeTest;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;

public class AuthorTemplateTest {
    Path outputDirectory;

    @BeforeTest
    public void setUp(ITestContext ctx) throws IOException {
        outputDirectory = Files.createTempDirectory("AuthorTemplateTest");
        outputDirectory.toFile().deleteOnExit();
    }

    @Test
    public void smokeTestAuthorTemplateCommand(){
        Cli.CliBuilder<Runnable> builder = createBuilder();
        String[] arguments = new String[]{
                "author",
                "template",
                "-g",
                "java",
                "--library",
                "webclient",
                "--output",
                outputDirectory.toAbsolutePath().toString()
        };
        builder.build().parse(arguments).run();

        // spot check root files
        Assertions.assertTrue(Files.exists(outputDirectory.resolve("ApiClient.mustache")));
        Assertions.assertTrue(Files.exists(outputDirectory.resolve("api_doc.mustache")));
        Assertions.assertTrue(Files.exists(outputDirectory.resolve("pom.mustache")));

        Assertions.assertTrue(Files.exists(outputDirectory.resolve("auth/OAuth.mustache")));

        // check libraries files and subdirectories
        Assertions.assertTrue(Files.exists(outputDirectory.resolve("libraries/webclient/ApiClient.mustache")));
        Assertions.assertTrue(Files.exists(outputDirectory.resolve("libraries/webclient/pom.mustache")));
        Assertions.assertTrue(Files.exists(outputDirectory.resolve("libraries/webclient/auth/OAuth.mustache")));

        // check non-existence of unselected libraries
        Assertions.assertFalse(Files.exists(outputDirectory.resolve("libraries/feign/build.gradle.mustache")));
        Assertions.assertFalse(Files.exists(outputDirectory.resolve("libraries/feign/auth/OAuth.mustache")));

        Assertions.assertFalse(Files.exists(outputDirectory.resolve("libraries/jersey2/api_doc.mustache")));
        Assertions.assertFalse(Files.exists(outputDirectory.resolve("libraries/jersey2/auth/HttpBasicAuth.mustache")));

        Assertions.assertFalse(Files.exists(outputDirectory.resolve("libraries/okhttp-gson/api.mustache")));
        Assertions.assertFalse(Files.exists(outputDirectory.resolve("libraries/okhttp-gson/auth/RetryingOAuth.mustache")));
    }

    private Cli.CliBuilder<Runnable> createBuilder(){
        Cli.CliBuilder<Runnable> builder = new Cli.CliBuilder<>("openapi-generator-cli");

        builder.withGroup("author")
                .withDescription("Utilities for authoring generators or customizing templates.")
                .withDefaultCommand(HelpCommand.class)
                .withCommands(AuthorTemplate.class);

        return builder;
    }
}
