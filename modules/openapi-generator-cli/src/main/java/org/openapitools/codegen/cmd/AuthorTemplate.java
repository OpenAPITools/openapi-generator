package org.openapitools.codegen.cmd;

import com.github.jknack.handlebars.internal.lang3.StringUtils;
import io.airlift.airline.Command;
import io.airlift.airline.Option;
import org.openapitools.codegen.CodegenConstants;

import java.nio.file.Path;
import java.util.Locale;
import java.util.regex.Pattern;

@SuppressWarnings({"FieldMayBeFinal", "FieldCanBeLocal", "unused"})
@Command(name = "template", description = "Retrieve templates for local modification")
public class AuthorTemplate extends OpenApiGeneratorCommand {

    @Option(name = {"-g", "--generator-name"}, title = "generator name",
            description = "generator to use (see list command for list)",
            required = true)
    private String generatorName;

    @Option(name = {"--library"}, title = "library", description = CodegenConstants.LIBRARY_DESC)
    private String library;

    private Pattern pattern = null;
    @Option(name = {"-o", "--output"}, title = "output directory",
            description = "where to write the template files (defaults to 'out')")
    private String output = "";

    @Option(name = {"-v", "--verbose"}, description = "verbose mode")
    private boolean verbose;

    @Override
    void execute() {
        TemplateExtractor templateExtractor = new TemplateExtractor(generatorName, library, output, verbose);
        templateExtractor.extractTemplates();
    }

    private boolean shouldCopy(Path relativePath) {
        String path = relativePath.toString();
        if (StringUtils.isNotEmpty(library) && path.contains("libraries")) {
            if (pattern == null) {
                pattern = Pattern.compile(String.format(Locale.ROOT, "libraries[/\\\\]{1}%s[/\\\\]{1}.*", Pattern.quote(library)));
            }

            return pattern.matcher(path).matches();
        }

        return true;
    }
}
