package io.swagger.generator.online;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotNull;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.codegen.options.JavaOptionsProvider;
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
