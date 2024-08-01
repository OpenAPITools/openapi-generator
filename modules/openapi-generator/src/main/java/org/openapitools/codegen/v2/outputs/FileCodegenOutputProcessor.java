package org.openapitools.codegen.v2.outputs;

import org.openapitools.codegen.v2.CodegenObject;
import org.openapitools.codegen.v2.CodegenObjectVisitor;
import org.openapitools.codegen.v2.reflection.GenericClass;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Objects;

public final class FileCodegenOutputProcessor implements CodegenOutputProcessor {
    private final Path basePath;
    private final Charset charset;

    public FileCodegenOutputProcessor() {
        this(Paths.get("."), StandardCharsets.UTF_8);
    }

    public FileCodegenOutputProcessor(Path basePath, Charset charset) {
        this.basePath = Objects.requireNonNull(basePath);
        this.charset = Objects.requireNonNull(charset);
    }

    public void process(CodegenObject object) {
        CodegenObjectVisitor visitor = o -> {
            Collection<CodegenOutput> outputs = o.getTagOrDefault(
                    CodegenOutputTags.OUTPUTS, new GenericClass<Collection<CodegenOutput>>() {}, ArrayList::new);
            for (CodegenOutput output : outputs) {
                try {
                    Path outputPath = getPathFromLocation(output.getLocation());
                    if (outputPath != null) {
                        Path fullPath = basePath.resolve(outputPath).toAbsolutePath();
                        Files.createDirectories(fullPath.getParent());
                        Files.write(fullPath, output.getContent().getBytes(charset));
                    }
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        };

        visitor.visit(object);
    }

    private Path getPathFromLocation(Object location) {
        if (location instanceof Path) {
            return (Path) location;
        }

        if (location instanceof File) {
            return ((File) location).toPath();
        }

        if (location instanceof String) {
            return Paths.get((String) location);
        }

        if (location instanceof URI) {
            return Paths.get((URI) location);
        }

        return null;
    }
}