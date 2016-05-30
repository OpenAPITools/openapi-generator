package io.swagger.codegen.ignore.rules;

import java.nio.file.*;
import java.util.List;

public class DirectoryRule extends FileRule {

    private PathMatcher directoryMatcher = null;
    private PathMatcher contentsMatcher = null;

    DirectoryRule(List<Part> syntax, String definition) {
        super(syntax, definition);
        String pattern = this.getPattern();
        StringBuilder sb = new StringBuilder();
        sb.append("glob:");
        sb.append(pattern);
        if(!pattern.endsWith("/")) sb.append("/");
        directoryMatcher = FileSystems.getDefault().getPathMatcher(sb.toString());
        sb.append("**");
        contentsMatcher = FileSystems.getDefault().getPathMatcher(sb.toString());
    }

    @Override
    public Boolean matches(String relativePath) {
        return contentsMatcher.matches(FileSystems.getDefault().getPath(relativePath)) || directoryMatcher.matches(FileSystems.getDefault().getPath(relativePath));
    }
}
