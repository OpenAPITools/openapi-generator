package io.swagger.generator.online;

import static org.testng.Assert.assertNotEquals;

import io.swagger.codegen.options.AkkaScalaClientOptionsProvider;
import io.swagger.codegen.options.AndroidClientOptionsProvider;
import io.swagger.codegen.options.AsyncScalaClientOptionsProvider;
import io.swagger.codegen.options.CSharpClientOptionsProvider;
import io.swagger.codegen.options.CsharpDotNet2ClientOptionsProvider;
import io.swagger.codegen.options.DartClientOptionsProvider;
import io.swagger.codegen.options.FlashClienOptionsProvider;
import io.swagger.codegen.options.JavaInflectorServerOptionsProvider;
import io.swagger.codegen.options.JavaOptionsProvider;
import io.swagger.codegen.options.JaxRSServerOptionsProvider;
import io.swagger.codegen.options.NodeJSServerOptionsProvider;
import io.swagger.codegen.options.ObjcClientOptionsProvider;
import io.swagger.codegen.options.OptionsProvider;
import io.swagger.codegen.options.PerlClientOptionsProvider;
import io.swagger.codegen.options.PhpClientOptionsProvider;
import io.swagger.codegen.options.PythonClientOptionsProvider;
import io.swagger.codegen.options.Qt5CPPOptionsProvider;
import io.swagger.codegen.options.RubyClientOptionsProvider;
import io.swagger.codegen.options.ScalaClientOptionsProvider;
import io.swagger.codegen.options.ScalatraServerOptionsProvider;
import io.swagger.codegen.options.SilexServerOptionsProvider;
import io.swagger.codegen.options.SinatraServerOptionsProvider;
import io.swagger.codegen.options.SpringMVCServerOptionsProvider;
import io.swagger.codegen.options.StaticDocOptionsProvider;
import io.swagger.codegen.options.StaticHtmlOptionsProvider;
import io.swagger.codegen.options.SwaggerOptionsProvider;
import io.swagger.codegen.options.SwaggerYamlOptionsProvider;
import io.swagger.codegen.options.SwiftOptionsProvider;
import io.swagger.codegen.options.TizenClientOptionsProvider;
import io.swagger.codegen.options.TypeScriptAngularClientOptionsProvider;
import io.swagger.codegen.options.TypeScriptNodeClientOptionsProvider;
import io.swagger.generator.exception.ApiException;
import io.swagger.generator.model.GeneratorInput;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Maps;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public class OnlineGeneratorOptionsTest {
    private static final String OPTIONS_PROVIDER = "optionsProvider";

    @DataProvider(name = OPTIONS_PROVIDER)
    private Object[][] listOptions() {
        return new Object[][]{{new AkkaScalaClientOptionsProvider()}, {new AndroidClientOptionsProvider()},
                {new AsyncScalaClientOptionsProvider()}, {new CSharpClientOptionsProvider()},
                {new CsharpDotNet2ClientOptionsProvider()}, {new DartClientOptionsProvider()},
                {new FlashClienOptionsProvider()}, {new JavaInflectorServerOptionsProvider()},
                {new JavaOptionsProvider()}, {new JaxRSServerOptionsProvider()},
                {new NodeJSServerOptionsProvider()}, {new ObjcClientOptionsProvider()},
                {new PerlClientOptionsProvider()}, {new PhpClientOptionsProvider()},
                {new PythonClientOptionsProvider()}, {new Qt5CPPOptionsProvider()},
                {new RubyClientOptionsProvider()}, {new ScalaClientOptionsProvider()},
                {new ScalatraServerOptionsProvider()}, {new SilexServerOptionsProvider()},
                {new SinatraServerOptionsProvider()}, {new SpringMVCServerOptionsProvider()},
                {new StaticDocOptionsProvider()}, {new StaticHtmlOptionsProvider()},
                {new SwaggerOptionsProvider()}, {new SwaggerYamlOptionsProvider()},
                {new SwiftOptionsProvider()}, {new TizenClientOptionsProvider()},
                {new TypeScriptAngularClientOptionsProvider()}, {new TypeScriptNodeClientOptionsProvider()},
        };
    }

    @Test(dataProvider = OPTIONS_PROVIDER)
    public void optionsTest(OptionsProvider provider) throws ApiException, IOException {
        final GeneratorInput input = new GeneratorInput();
        final HashMap<String, InvocationCounter> options = convertOptions(provider);

        final Maps.EntryTransformer<String, InvocationCounter, String> transformer =
                new Maps.EntryTransformer<String, InvocationCounter, String>() {
                    public String transformEntry(String key, InvocationCounter value) {
                        return value.getValue();
                    }
                };

        input.setOptions(Maps.transformEntries(options, transformer));
        final ObjectMapper mapper = new ObjectMapper();
        input.setSpec(mapper.readTree(loadClassResource(getClass(), "petstore.json")));
        String outputFilename;
        if (provider.isServer()) {
            outputFilename = Generator.generateServer(provider.getLanguage(), input);
        } else {
            outputFilename = Generator.generateClient(provider.getLanguage(), input);
        }
        final File dir = new File(new File(outputFilename).getParent());
        FileUtils.deleteDirectory(dir);
        for (InvocationCounter option : options.values()) {
            assertNotEquals(option.getCounter(), 0, String.format("Option \"%s\" wasn't processed.",
                    option.getValue()));
        }
    }

    private HashMap<String, InvocationCounter> convertOptions(OptionsProvider provider) {
        HashMap<String, InvocationCounter> options = new HashMap<String, InvocationCounter>();
        for (Map.Entry<String, String> entry : provider.createOptions().entrySet()) {
            options.put(entry.getKey(), new InvocationCounter(entry.getValue()));
        }
        return options;
    }

    private static String loadClassResource(Class<?> cls, String name) throws IOException {
        InputStream in = null;
        try {
            in = cls.getClassLoader().getResourceAsStream(name);
            return IOUtils.toString(in, StandardCharsets.UTF_8);
        } finally {
            IOUtils.closeQuietly(in);
        }
    }

    private static class InvocationCounter {
        private String value;
        private int counter;

        public InvocationCounter(String value) {
            this.value = value;
        }

        public int getCounter() {
            return counter;
        }

        public String getValue() {
            ++counter;
            return value;
        }
    }
}
