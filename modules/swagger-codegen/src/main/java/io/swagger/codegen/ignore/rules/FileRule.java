package io.swagger.codegen.ignore.rules;

import java.nio.file.FileSystems;
import java.nio.file.PathMatcher;
import java.util.List;

public class FileRule extends Rule {

    private PathMatcher matcher = null;

    FileRule(List<Part> syntax, String definition) {
        super(syntax, definition);
        matcher = FileSystems.getDefault().getPathMatcher("glob:"+this.getPattern());
    }

    @Override
    public Boolean matches(String relativePath) {
        return matcher.matches(FileSystems.getDefault().getPath(relativePath));
    }
}
