package io.swagger.codegen.ignore.rules;

import java.util.List;

public abstract class Rule {

    public enum Operation {EXCLUDE, INCLUDE, NOOP, EXCLUDE_AND_TERMINATE}

    // The original rule
    private final String definition;

    private final List<Part> syntax;

    Rule(List<Part> syntax, String definition) {
        this.syntax = syntax;
        this.definition = definition;
    }

    public abstract Boolean matches(String relativePath);

    public String getDefinition() {
        return this.definition;
    }

    protected String getPattern() {
        if(syntax == null) return this.definition;

        StringBuilder sb = new StringBuilder();
        for (Part current : syntax) {
            switch (current.getToken()) {
                case MATCH_ALL:
                case MATCH_ANY:
                case ESCAPED_EXCLAMATION:
                case ESCAPED_SPACE:
                case PATH_DELIM:
                case TEXT:
                case DIRECTORY_MARKER:
                    sb.append(current.getValue());
                    break;
                case NEGATE:
                case ROOTED_MARKER:
                case COMMENT:
                    break;
            }
        }
        return sb.toString();
    }

    /**
     * Whether or not the rule should be negated. !foo means foo should be removed from previous matches.
     * Example: **\/*.bak excludes all backup. Adding !/test.bak will include test.bak in the project root.
     * <p>
     * NOTE: It is not possible to re-include a file if a parent directory of that file is excluded.
     *
     * @return {@code true} if the rule is negated (inverse), otherwise {@code false} (normal).
     */
    public Boolean getNegated() {
        return this.syntax != null && this.syntax.size() > 0 && this.syntax.get(0).getToken() == IgnoreLineParser.Token.NEGATE;
    }

    public Operation evaluate(String relativePath) {
        if (Boolean.TRUE.equals(matches(relativePath))) {
            if(Boolean.TRUE.equals(this.getNegated())) {
                return this.getIncludeOperation();
            }
            return this.getExcludeOperation();
        }
        return Operation.NOOP;
    }

    protected Operation getIncludeOperation(){ return Operation.INCLUDE; }
    protected Operation getExcludeOperation(){ return Operation.EXCLUDE; }

    public static Rule create(String definition) {
        // NOTE: Comments that start with a : (e.g. //:) are pulled from git documentation for .gitignore
        // see: https://github.com/git/git/blob/90f7b16b3adc78d4bbabbd426fb69aa78c714f71/Documentation/gitignore.txt
        Rule rule = null;
        if (definition.equals(".")) {
            return new InvalidRule(null, definition, "Pattern '.' is invalid.");
        } else if (definition.equals("!.")) {
            return new InvalidRule(null, definition, "Pattern '!.' is invalid.");
        } else if (definition.startsWith("..")) {
            return new InvalidRule(null, definition, "Pattern '..' is invalid.");
        }

        try {
            List<Part> result = IgnoreLineParser.parse(definition);

            Boolean directoryOnly = null;
            if (result.size() == 0) {
                return rule;
            } else if (result.size() == 1) {
                // single-character filename only
                Part part = result.get(0);
                if (IgnoreLineParser.Token.MATCH_ANY.equals(part.getToken())) {
                    rule = new RootedFileRule(result, definition);
                } else {
                    rule = new FileRule(result, definition);
                }
            } else {
                IgnoreLineParser.Token head = result.get(0).getToken();

                //: An optional prefix "`!`" which negates the pattern; any
                //: matching file excluded by a previous pattern will become
                //: included again. It is not possible to re-include a file if a parent
                //: directory of that file is excluded. Git doesn't list excluded
                //: directories for performance reasons, so any patterns on contained
                //: files have no effect, no matter where they are defined.
                //: Put a backslash ("`\`") in front of the first "`!`" for patterns
                //: that begin with a literal "`!`", for example, "`\!important!.txt`".
                // see this.getNegated();

                //: If the pattern ends with a slash, it is removed for the
                //: purpose of the following description, but it would only find
                //: a match with a directory.  In other words, `foo/` will match a
                //: directory `foo` and paths underneath it, but will not match a
                //: regular file or a symbolic link `foo` (this is consistent
                //: with the way how pathspec works in general in Git).
                directoryOnly = IgnoreLineParser.Token.DIRECTORY_MARKER.equals(result.get(result.size() - 1).getToken());

                if (directoryOnly) {
                    rule = new DirectoryRule(result, definition);
                } else if (IgnoreLineParser.Token.PATH_DELIM.equals(head)) {
                    //: A leading slash matches the beginning of the pathname.
                    //: For example, "/{asterisk}.c" matches "cat-file.c" but not
                    //: "mozilla-sha1/sha1.c".
                    rule = new RootedFileRule(result, definition);
                } else {
                    // case 1
                    //: If the pattern does not contain a slash '/', Git treats it as
                    //: a shell glob pattern and checks for a match against the
                    //: pathname relative to the location of the `.gitignore` file
                    //: (relative to the toplevel of the work tree if not from a
                    //: `.gitignore` file).

                    // case 2
                    //: Otherwise, Git treats the pattern as a shell glob suitable
                    //: for consumption by fnmatch(3) with the FNM_PATHNAME flag:
                    //: wildcards in the pattern will not match a / in the pathname.
                    //: For example, "Documentation/{asterisk}.html" matches
                    //: "Documentation/git.html" but not "Documentation/ppc/ppc.html"
                    //: or "tools/perf/Documentation/perf.html".


                    // case 3
                    //: Two consecutive asterisks ("`**`") in patterns matched against
                    //: full pathname may have special meaning:
                    //:
                    //: - A leading "`**`" followed by a slash means match in all
                    //: directories. For example, "`**/foo`" matches file or directory
                    //: "`foo`" anywhere, the same as pattern "`foo`". "`**/foo/bar`"
                    //: matches file or directory "`bar`" anywhere that is directly
                    //: under directory "`foo`".
                    //:
                    //: - A trailing "`/**`" matches everything inside. For example,
                    //: "`abc/**`" matches all files inside directory "`abc`", relative
                    //: to the location of the `.gitignore` file, with infinite depth.
                    //:
                    //: - A slash followed by two consecutive asterisks then a slash
                    //: matches zero or more directories. For example, "`a/**/b`"
                    //: matches "`a/b`", "`a/x/b`", "`a/x/y/b`" and so on.
                    //:
                    //: - Other consecutive asterisks are considered invalid.
                    rule = new FileRule(result, definition);
                }

            }
        } catch (ParserException e) {
            e.printStackTrace();
            return new InvalidRule(null, definition, e.getMessage());
        }

        return rule;
    }
}
