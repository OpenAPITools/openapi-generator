package io.swagger.codegen.ruby;

import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.languages.RubyClientCodegen;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import java.io.File;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.junit.Assert.fail;
import static org.testng.Assert.*;

/**
 * Tests for RubyClientCodegen-generated templates
 */
public class RubyClientCodegenTest {

  public TemporaryFolder folder = new TemporaryFolder();

  @BeforeMethod
  public void setUp() throws Exception {
      folder.create();
  }

  @AfterMethod
  public void tearDown() throws Exception {
      folder.delete();
  }

  @Test
  public void testGenerateRubyClientWithHtmlEntity() throws Exception {
      final File output = folder.getRoot();

      final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/pathWithHtmlEntity.yaml");
      CodegenConfig codegenConfig = new RubyClientCodegen();
      codegenConfig.setOutputDir(output.getAbsolutePath());

      ClientOptInput clientOptInput = new ClientOptInput().opts(new ClientOpts()).swagger(swagger).config(codegenConfig);

      DefaultGenerator generator = new DefaultGenerator();
      List<File> files = generator.opts(clientOptInput).generate();
      boolean apiFileGenerated = false;
      for (File file : files) {
        if (file.getName().equals("default_api.rb")) {
          apiFileGenerated = true;
          // Ruby client should set the path unescaped in the api file
          assertTrue(FileUtils.readFileToString(file, StandardCharsets.UTF_8).contains("local_var_path = '/foo=bar'"));
        }
      }
      if (!apiFileGenerated) {
        fail("Default api file is not generated!");
      }
  }

}
