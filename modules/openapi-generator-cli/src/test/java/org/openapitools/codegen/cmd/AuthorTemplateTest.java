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
        Assert.assertTrue(Files.exists(outputDirectory.resolve("ApiClient.mustache")));
        Assert.assertTrue(Files.exists(outputDirectory.resolve("api_doc.mustache")));
        Assert.assertTrue(Files.exists(outputDirectory.resolve("pom.mustache")));

        Assert.assertTrue(Files.exists(outputDirectory.resolve("auth/OAuth.mustache")));

        // check libraries files and subdirectories
        Assert.assertTrue(Files.exists(outputDirectory.resolve("libraries/webclient/ApiClient.mustache")));
        Assert.assertTrue(Files.exists(outputDirectory.resolve("libraries/webclient/pom.mustache")));
        Assert.assertTrue(Files.exists(outputDirectory.resolve("libraries/webclient/auth/OAuth.mustache")));

        // check non-existence of unselected libraries
        Assert.assertFalse(Files.exists(outputDirectory.resolve("libraries/feign/build.gradle.mustache")));
        Assert.assertFalse(Files.exists(outputDirectory.resolve("libraries/feign/auth/OAuth.mustache")));

        Assert.assertFalse(Files.exists(outputDirectory.resolve("libraries/jersey2/api_doc.mustache")));
        Assert.assertFalse(Files.exists(outputDirectory.resolve("libraries/jersey2/auth/HttpBasicAuth.mustache")));

        Assert.assertFalse(Files.exists(outputDirectory.resolve("libraries/okhttp-gson/api.mustache")));
        Assert.assertFalse(Files.exists(outputDirectory.resolve("libraries/okhttp-gson/auth/RetryingOAuth.mustache")));
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
