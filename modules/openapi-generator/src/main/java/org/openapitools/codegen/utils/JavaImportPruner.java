package org.openapitools.codegen.utils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Removes unused and duplicate imports from Java source code.
 * <p>
 * The analysis is lexical: comments and string/char literals are masked, and an import is considered used when
 * its simple name occurs as an unqualified identifier in the remaining code. A single-type import only introduces
 * its simple name into scope, so removing an import whose simple name is never used cannot change the meaning of
 * the code. Wildcard imports and imports referenced from Javadoc (e.g. {@code {@link Foo}}) are kept.
 * <p>
 * Additionally, imports whose simple name clashes with a top-level type declared in the same file are removed,
 * as they would not compile.
 */
public final class JavaImportPruner {

    private static final String IDENTIFIER = "[\\p{L}_$][\\p{L}\\p{N}_$]*";
    private static final Pattern IMPORT = Pattern.compile(
            "^[ \\t]*import[ \\t]+(static[ \\t]+)?([\\p{L}\\p{N}_$.]+?)(\\.\\*)?[ \\t]*;[ \\t]*(?:\\R|$)", Pattern.MULTILINE);
    private static final Pattern PACKAGE = Pattern.compile("^[ \\t]*package[ \\t]+([\\p{L}\\p{N}_$.]+)[ \\t]*;", Pattern.MULTILINE);
    private static final Pattern IDENTIFIER_PATTERN = Pattern.compile(IDENTIFIER);
    private static final Pattern JAVADOC_REFERENCE = Pattern.compile(
            "(?:\\{@(?:link|linkplain|value)|@see|@throws|@exception)\\s+#?(" + IDENTIFIER + ")");
    private static final Pattern TYPE_DECLARATION = Pattern.compile("\\b(?:class|interface|enum|record)\\s+(" + IDENTIFIER + ")");
    private static final Pattern EXCESS_BLANK_LINES = Pattern.compile("\\n(?:[ \\t]*\\r?\\n){2,}");

    private JavaImportPruner() {
    }

    /**
     * Removes unused and duplicate imports from the given Java source.
     *
     * @param source Java source code
     * @return the source without unused and duplicate imports, or the unchanged source if there are none
     */
    public static String removeUnusedImports(String source) {
        if (source == null || !source.contains("import")) {
            return source;
        }
        final StringBuilder comments = new StringBuilder();
        final String code = maskCommentsAndLiterals(source, comments);

        final List<ImportStatement> imports = new ArrayList<>();
        final Matcher importMatcher = IMPORT.matcher(code);
        while (importMatcher.find()) {
            imports.add(new ImportStatement(importMatcher));
        }
        if (imports.isEmpty()) {
            return source;
        }

        final String packageName = findPackageName(code);
        final String body = blankOut(code, imports, PACKAGE.matcher(code));
        final Set<String> usedNames = findUnqualifiedIdentifiers(body);
        final Set<String> javadocNames = findJavadocReferences(comments);
        final Set<String> declaredTypes = findTopLevelTypeNames(body);

        final List<ImportStatement> removals = new ArrayList<>();
        final Set<String> seen = new HashSet<>();
        for (ImportStatement statement : imports) {
            if (isRemovable(statement, packageName, seen, usedNames, javadocNames, declaredTypes)) {
                removals.add(statement);
            }
        }
        if (removals.isEmpty()) {
            return source;
        }
        return removeImports(source, removals, imports.get(0).start);
    }

    private static boolean isRemovable(ImportStatement statement, String packageName, Set<String> seen,
                                       Set<String> usedNames, Set<String> javadocNames, Set<String> declaredTypes) {
        if (!seen.add(statement.key())) {
            return true; // duplicate
        }
        if (statement.isWildcard) {
            return false;
        }
        final String simpleName = statement.simpleName();
        if (!statement.isStatic && declaredTypes.contains(simpleName)
                && !statement.qualifiedName.equals(packageName + "." + simpleName)) {
            return true; // clashes with a type declared in this file
        }
        return !usedNames.contains(simpleName) && !javadocNames.contains(simpleName);
    }

    /**
     * Replaces comments, string/char literals and text blocks with spaces (keeping line breaks), so that the
     * returned code has the same length and offsets as the source. The comments are collected separately.
     */
    private static String maskCommentsAndLiterals(String source, StringBuilder comments) {
        final char[] code = source.toCharArray();
        int i = 0;
        while (i < code.length) {
            final char c = code[i];
            final int end;
            if (c == '/' && i + 1 < code.length && code[i + 1] == '/') {
                end = indexOfOrEnd(source, "\n", i);
                comments.append(source, i, end).append('\n');
            } else if (c == '/' && i + 1 < code.length && code[i + 1] == '*') {
                end = indexOfOrEnd(source, "*/", i + 2) + 2;
                comments.append(source, i, Math.min(end, code.length)).append('\n');
            } else if (c == '"' && source.startsWith("\"\"\"", i)) {
                end = endOfLiteral(source, i + 3, "\"\"\"");
            } else if (c == '"' || c == '\'') {
                end = endOfLiteral(source, i + 1, String.valueOf(c));
            } else {
                i++;
                continue;
            }
            for (int j = i; j < Math.min(end, code.length); j++) {
                if (code[j] != '\n' && code[j] != '\r') {
                    code[j] = ' ';
                }
            }
            i = end;
        }
        return new String(code);
    }

    private static int indexOfOrEnd(String source, String token, int from) {
        final int index = source.indexOf(token, from);
        return index < 0 ? source.length() : index;
    }

    private static int endOfLiteral(String source, int from, String delimiter) {
        int i = from;
        while (i < source.length()) {
            if (source.charAt(i) == '\\') {
                i += 2;
            } else if (source.startsWith(delimiter, i)) {
                return i + delimiter.length();
            } else if (delimiter.length() == 1 && source.charAt(i) == '\n') {
                return i; // unterminated literal, stop at the end of the line
            } else {
                i++;
            }
        }
        return source.length();
    }

    private static String findPackageName(String code) {
        final Matcher m = PACKAGE.matcher(code);
        return m.find() ? m.group(1) : "";
    }

    private static String blankOut(String code, List<ImportStatement> imports, Matcher packageMatcher) {
        final char[] chars = code.toCharArray();
        for (ImportStatement statement : imports) {
            blankOut(chars, statement.start, statement.end);
        }
        if (packageMatcher.find()) {
            blankOut(chars, packageMatcher.start(), packageMatcher.end());
        }
        return new String(chars);
    }

    private static void blankOut(char[] chars, int start, int end) {
        for (int j = start; j < end; j++) {
            if (chars[j] != '\n' && chars[j] != '\r') {
                chars[j] = ' ';
            }
        }
    }

    /**
     * Collects identifiers that are not qualified, i.e. not preceded by a dot. A qualified name such as
     * {@code java.util.Locale.ROOT} does not need an import of {@code Locale}.
     */
    private static Set<String> findUnqualifiedIdentifiers(String body) {
        final Set<String> names = new HashSet<>();
        final Matcher m = IDENTIFIER_PATTERN.matcher(body);
        while (m.find()) {
            int p = m.start() - 1;
            while (p >= 0 && Character.isWhitespace(body.charAt(p))) {
                p--;
            }
            if (p < 0 || body.charAt(p) != '.') {
                names.add(m.group());
            }
        }
        return names;
    }

    private static Set<String> findJavadocReferences(CharSequence comments) {
        final Set<String> names = new HashSet<>();
        final Matcher m = JAVADOC_REFERENCE.matcher(comments);
        while (m.find()) {
            names.add(m.group(1));
        }
        return names;
    }

    private static Set<String> findTopLevelTypeNames(String body) {
        final Set<String> names = new HashSet<>();
        final Matcher m = TYPE_DECLARATION.matcher(body);
        int depth = 0;
        int position = 0;
        while (m.find()) {
            for (; position < m.start(); position++) {
                final char c = body.charAt(position);
                if (c == '{') {
                    depth++;
                } else if (c == '}') {
                    depth--;
                }
            }
            if (depth == 0 && !isPrecededByDot(body, m.start())) {
                names.add(m.group(1));
            }
        }
        return names;
    }

    private static boolean isPrecededByDot(String body, int index) {
        int p = index - 1;
        while (p >= 0 && Character.isWhitespace(body.charAt(p))) {
            p--;
        }
        return p >= 0 && body.charAt(p) == '.';
    }

    /**
     * Removes the given imports (in source order) and collapses blank lines left behind in the import section.
     */
    private static String removeImports(String source, List<ImportStatement> removals, int importSectionStart) {
        final StringBuilder result = new StringBuilder(source.length());
        int position = 0;
        for (ImportStatement statement : removals) {
            result.append(source, position, statement.start);
            position = statement.end;
        }
        final int importSectionEnd = result.length();
        result.append(source, position, source.length());

        // collapse blank lines in the import section, including the blank lines following it
        int end = importSectionEnd;
        while (end < result.length() && Character.isWhitespace(result.charAt(end))) {
            end++;
        }
        final int start = Math.max(0, result.lastIndexOf("\n", importSectionStart));
        final String section = result.substring(start, end);
        final String collapsed = EXCESS_BLANK_LINES.matcher(section).replaceAll("\n\n");
        return result.replace(start, end, collapsed).toString();
    }

    private static final class ImportStatement {
        private final int start;
        private final int end;
        private final boolean isStatic;
        private final boolean isWildcard;
        private final String qualifiedName;

        private ImportStatement(Matcher match) {
            this.start = match.start();
            this.end = match.end();
            this.isStatic = match.group(1) != null;
            this.qualifiedName = match.group(2);
            this.isWildcard = match.group(3) != null;
        }

        private String simpleName() {
            return qualifiedName.substring(qualifiedName.lastIndexOf('.') + 1);
        }

        private String key() {
            return (isStatic ? "static " : "") + qualifiedName + (isWildcard ? ".*" : "");
        }
    }
}
