package io.swagger.codegen.ignore.rules;

import java.nio.file.FileSystems;
import java.nio.file.PathMatcher;
import java.util.List;

public class DirectoryRule extends FileRule {

    private PathMatcher matcher = null;

    DirectoryRule(List<Part> syntax, String definition) {
        super(syntax, definition);
        matcher = FileSystems.getDefault().getPathMatcher("glob:**/"+this.getPattern());
    }

    @Override
    public Boolean matches(String relativePath) {
        return matcher.matches(FileSystems.getDefault().getPath(relativePath));
    }
}
