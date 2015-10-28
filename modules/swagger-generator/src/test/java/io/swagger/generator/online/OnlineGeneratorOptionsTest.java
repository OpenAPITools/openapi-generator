package io.swagger.generator.online;

import static org.testng.Assert.assertNotEquals;

import io.swagger.generator.exception.ApiException;
import io.swagger.generator.model.GeneratorInput;
import io.swagger.generator.online.Generator;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.common.collect.Maps;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public abstract class OnlineGeneratorOptionsTest {
    private final boolean isServer;
    private final String language;

    protected OnlineGeneratorOptionsTest(String language, boolean isServer) {
        this.language = language;
        this.isServer = isServer;
    }

    @Test
    public void optionsTest() throws ApiException, IOException {
        final GeneratorInput input = new GeneratorInput();
        final HashMap<String, InvocationCounter> options = convertOptions();

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
        if (isServer) {
            outputFilename = Generator.generateServer(language, input);
        } else {
            outputFilename = Generator.generateClient(language, input);
        }
        final File dir = new File(new File(outputFilename).getParent());
        FileUtils.deleteDirectory(dir);
        for (InvocationCounter option : options.values()) {
            assertNotEquals(option.getCounter(), 0, String.format("Option \"%s\" wasn't processed.",
                    option.getValue()));
        }
    }

    protected abstract Map<String, String> getOptions();

    private HashMap<String, InvocationCounter> convertOptions() {
        HashMap<String, InvocationCounter> options = new HashMap<String, InvocationCounter>();
        for (Map.Entry<String, String> entry : getOptions().entrySet()) {
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
