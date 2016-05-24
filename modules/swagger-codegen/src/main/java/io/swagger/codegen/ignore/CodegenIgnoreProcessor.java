package io.swagger.codegen.ignore;

import com.google.common.collect.ImmutableList;
import io.swagger.codegen.ignore.rules.DirectoryRule;
import io.swagger.codegen.ignore.rules.Rule;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

public class CodegenIgnoreProcessor {

    private static final Logger LOGGER = LoggerFactory.getLogger(CodegenIgnoreProcessor.class);
    private final String outputPath;
    private List<Rule> exclusionRules = new ArrayList<>();
    private List<Rule> inclusionRules = new ArrayList<>();

    public CodegenIgnoreProcessor(String outputPath) {
        this.outputPath = outputPath;
        final File directory = new File(outputPath);
        if(directory.exists() && directory.isDirectory()){
            final File codegenIgnore = new File(directory, ".swagger-codegen-ignore");
            if(codegenIgnore.exists() && codegenIgnore.isFile()){
                try {
                    loadCodegenRules(codegenIgnore);
                } catch (IOException e) {
                    LOGGER.error("Could not process .swagger-codegen-ignore.", e.getMessage());
                }
            } else {
                // log info message
                LOGGER.info("No .swagger-codegen-ignore file found.");
            }
        }
    }

    void loadCodegenRules(File codegenIgnore) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(codegenIgnore))) {
            String line;

            // NOTE: Comments that start with a : (e.g. //:) are pulled from git documentation for .gitignore
            // see: https://github.com/git/git/blob/90f7b16b3adc78d4bbabbd426fb69aa78c714f71/Documentation/gitignore.txt
            while ((line = reader.readLine()) != null) {
                if(
                    //: A blank line matches no files, so it can serve as a separator for readability.
                    line.length() == 0
                ) continue;

                Rule rule = Rule.create(line);

                // rule could be null here if it's a COMMENT, for example
                if(rule != null) {
                    if (Boolean.TRUE.equals(rule.getNegated())) {
                        inclusionRules.add(rule);
                    } else {
                        exclusionRules.add(rule);
                    }
                }
            }
        }
    }

    public boolean allowsFile(File targetFile) {
        File file = new File(new File(this.outputPath).toURI().relativize(targetFile.toURI()).getPath());
        Boolean directoryExcluded = false;
        Boolean exclude = false;
        if(exclusionRules.size() == 0 && inclusionRules.size() == 0) {
            return true;
        }

        // NOTE: We *must* process all exclusion rules
        for (int i = 0; i < exclusionRules.size(); i++) {
            Rule current = exclusionRules.get(i);
            Rule.Operation op = current.evaluate(file.getPath());

            switch (op){
                case EXCLUDE:
                    exclude = true;

                    // Include rule can't override rules that exclude a file by some parent directory.
                    if(current instanceof DirectoryRule) {
                        directoryExcluded = true;
                    }
                    break;
                case INCLUDE:
                    // This won't happen here.
                    break;
                case NOOP:
                    break;
                case EXCLUDE_AND_TERMINATE:
                    i = exclusionRules.size();
                    break;
            }
        }

        if(exclude) {
            // Only need to process inclusion rules if we've been excluded
            for (int i = 0; exclude && i < inclusionRules.size(); i++) {
                Rule current = inclusionRules.get(i);
                Rule.Operation op = current.evaluate(file.getPath());

                // At this point exclude=true means the file should be ignored.
                // op == INCLUDE means we have to flip that flag.
                if(op.equals(Rule.Operation.INCLUDE)) {
                    if(current instanceof DirectoryRule && directoryExcluded) {
                        // e.g
                        // baz/
                        // !foo/bar/baz/
                        // NOTE: Possibly surprising side effect:
                        // foo/bar/baz/
                        // !bar/
                        exclude = false;
                    } else if (!directoryExcluded) {
                        // e.g.
                        // **/*.log
                        // !ISSUE_1234.log
                        exclude = false;
                    }
                }
            }
        }

        return Boolean.FALSE.equals(exclude);
    }

    public List<Rule> getInclusionRules() {
        return ImmutableList.copyOf(inclusionRules);
    }

    public List<Rule> getExclusionRules() {
        return ImmutableList.copyOf(exclusionRules);
    }
}
