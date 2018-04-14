package io.swagger.generator.online;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.openapitools.codegen.options.JavaOptionsProvider;
import io.swagger.generator.exception.ApiException;

import org.testng.annotations.Test;

import java.util.Map;

@SuppressWarnings("static-method")
public class OnlineJavaClientOptionsTest {

    @Test
    public void getOptionsTest() throws ApiException {
        final Map<String, CliOption> options =
                Generator.getOptions(new JavaOptionsProvider().getLanguage());
        assertNotNull(options);
        final CliOption opt = options.get(CodegenConstants.LIBRARY);
        assertNotNull(opt);
        assertEquals(opt.getDefault(), "okhttp-gson");
    }
}
