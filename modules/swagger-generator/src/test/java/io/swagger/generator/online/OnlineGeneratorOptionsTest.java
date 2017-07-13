package io.swagger.generator.online;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.base.Function;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.codegen.options.*;
import io.swagger.generator.exception.ApiException;
import io.swagger.generator.model.GeneratorInput;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import javax.annotation.Nullable;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;

public class OnlineGeneratorOptionsTest {
    private static final String OPTIONS_PROVIDER = "optionsProvider";

    @DataProvider(name = OPTIONS_PROVIDER)
    private static Object[][] listOptions() {
        return new Object[][] {
                {new AkkaScalaClientOptionsProvider()},
                {new AndroidClientOptionsProvider()},
                {new AsyncScalaClientOptionsProvider()},
                {new CSharpClientOptionsProvider()},
                {new CsharpDotNet2ClientOptionsProvider()},
                {new DartClientOptionsProvider()},
                {new FlashClienOptionsProvider()},
                {new JavaInflectorServerOptionsProvider()},
                {
                        new JavaOptionsProvider(),
                        new JavaOptionsProvider(ImmutableMap.of(CodegenConstants.LIBRARY,
                                JavaClientCodegen.RETROFIT_2)),
                        new JavaOptionsProvider(
                                ImmutableMap.of(CodegenConstants.LIBRARY,
                                        JavaClientCodegen.RETROFIT_2,
                                        JavaClientCodegen.USE_RX_JAVA, "true"))},
                {new JaxRSServerOptionsProvider()}, {new NodeJSServerOptionsProvider()},
                {new ObjcClientOptionsProvider()}, {new PerlClientOptionsProvider()},
                {new PhpClientOptionsProvider()}, {new PythonClientOptionsProvider()},
                {new Qt5CPPOptionsProvider()}, {new RubyClientOptionsProvider()},
                {new ScalaClientOptionsProvider()}, {new ScalatraServerOptionsProvider()},
                {new SilexServerOptionsProvider()}, {new SinatraServerOptionsProvider()},
                {new SlimFrameworkServerOptionsProvider()}, {new SpringOptionsProvider()},
                {new StaticDocOptionsProvider()}, {new StaticHtmlOptionsProvider()},
                {new SwaggerOptionsProvider()}, {new SwaggerYamlOptionsProvider()},
                {new SwiftOptionsProvider()}, {new TizenClientOptionsProvider()},
                {new TypeScriptAngularClientOptionsProvider()},
                {new TypeScriptNodeClientOptionsProvider()}, {new LumenServerOptionsProvider()}};
    }

    @Test(dataProvider = OPTIONS_PROVIDER)
    public void generateOptionsTest(OptionsProvider provider) throws ApiException, IOException {
        final GeneratorInput input = new GeneratorInput();
        final HashMap<String, InvocationCounter> options = convertOptions(provider);

        final Maps.EntryTransformer<String, InvocationCounter, String> transformer =
                new Maps.EntryTransformer<String, InvocationCounter, String>() {
                    @Override
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

        try {
            FileUtils.deleteDirectory(dir);
        } catch (Exception e) { // directory can't be deleted for some reasons
            e.printStackTrace();
        }
        for (InvocationCounter option : options.values()) {
            assertNotEquals(option.getCounter(), 0,
                    String.format("Option \"%s\" wasn't processed.", option.getValue()));
        }
    }

    private static HashMap<String, InvocationCounter> convertOptions(OptionsProvider provider) {
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

    @Test(dataProvider = OPTIONS_PROVIDER)
    public static void getOptionsTest(OptionsProvider provider) throws ApiException {
        final Map<String, CliOption> opts = Generator.getOptions(provider.getLanguage());

        final Function<CliOption, CliOptionProxy> cliOptionWrapper =
                new Function<CliOption, CliOptionProxy>() {
                    @Nullable
                    @Override
                    public CliOptionProxy apply(@Nullable CliOption option) {
                        return new CliOptionProxy(option);
                    }
                };

        final List<CliOptionProxy> actual =
                Lists.transform(new ArrayList<CliOption>(opts.values()), cliOptionWrapper);
        final List<CliOptionProxy> expected =
                Lists.transform(CodegenConfigLoader.forName(provider.getLanguage()).cliOptions(),
                        cliOptionWrapper);
        assertEquals(actual, expected);
    }

    protected static class CliOptionProxy {
        private final CliOption wrapped;

        public CliOptionProxy(CliOption wrapped) {
            this.wrapped = wrapped;
        }

        public CliOption getWrapped() {
            return wrapped;
        }

        @Override
        public int hashCode() {
            return Objects.hash(wrapped.getOpt(), wrapped.getDescription(), wrapped.getType(),
                    wrapped.getDefault(), wrapped.getEnum());
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof CliOptionProxy) {
                final CliOption that = ((CliOptionProxy) obj).getWrapped();
                return Objects.equals(wrapped.getOpt(), that.getOpt())
                        && Objects.equals(wrapped.getDescription(), that.getDescription())
                        && Objects.equals(wrapped.getType(), that.getType())
                        && Objects.equals(wrapped.getDefault(), that.getDefault())
                        && Objects.equals(wrapped.getEnum(), that.getEnum());
            }
            return false;
        }
    }
}
