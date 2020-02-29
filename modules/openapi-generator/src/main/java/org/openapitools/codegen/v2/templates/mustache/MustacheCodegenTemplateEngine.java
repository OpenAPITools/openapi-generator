package org.openapitools.codegen.v2.templates.mustache;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import org.openapitools.codegen.v2.CodegenObject;
import org.openapitools.codegen.v2.CodegenTag;
import org.openapitools.codegen.v2.templates.CodegenTemplate;
import org.openapitools.codegen.v2.templates.CodegenTemplateEngine;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

public class MustacheCodegenTemplateEngine implements CodegenTemplateEngine {

    private final Mustache.Compiler mustacheCompiler;
    private final Path templateDirectory;

    public MustacheCodegenTemplateEngine() {
        this(null);
    }

    public MustacheCodegenTemplateEngine(Path templateDirectory) {
        this.mustacheCompiler = Mustache.compiler();
        this.templateDirectory = templateDirectory;
    }

    @Override
    public String process(CodegenObject object, CodegenTemplate template) {
        Template mustacheTemplate = mustacheCompiler
                .withLoader(this::openTemplateFile)
                .withCollector(new MustacheCodegenCollector())
                .escapeHTML(false)
                .defaultValue("")
                .compile(openTemplateFile(template.getTemplateFile()));

        return mustacheTemplate.execute(object);
    }

    private Reader openTemplateFile(String templateFile) {
        try {
            Path path = getTemplateFilePath(templateFile);
            byte[] content = Files.readAllBytes(path);
            return new StringReader(new String(content, StandardCharsets.UTF_8));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private Path getTemplateFilePath(String templateFile) {
        if (templateDirectory != null) {
            return templateDirectory.resolve(templateFile);
        }

        try {
            return Paths.get(ClassLoader.getSystemResource(templateFile).toURI());
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
    }

    private Map<String, Object> buildContext(CodegenObject object) {
        ObjectMapper mapper = new ObjectMapper();
        mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        Map<String, Object> context = mapper.convertValue(object, new TypeReference<Map<String, Object>>() {});
        for (Map.Entry<CodegenTag, Object> entry : object.getTags().entrySet()) {
            context.putIfAbsent(entry.getKey().getName(), entry.getValue());
        }
        return context;
    }
}
