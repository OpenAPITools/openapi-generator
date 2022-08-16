package org.openapitools.codegen.typescript;

/**
 * TestNG supports grouping using <a href="https://testng.org/doc/documentation-main.html#test-groups">Test Groups</a>.
 * <br>
 * <p>
 *     Using groups also enables running only a subset of tests, e.g. only for the generator you are currently working on.
 * </p>
 * <p>
 *     This speeds up development <strong>a lot</strong>, especially if run from an IDE with support for TestNG groups
 *     (e.g. IntelliJ IDEA and derivatives, VS Code, and lots of others)
 * </p>
 * <br>
 * <p>Suggested groups/group-names are:</p>
 * <ul>
 *     <li>one per language (e.g.: "typescript") since most generators for one language share lots of code</li>
 *     <li>one per language + generator  (i.e. the generator name, e.g.: "typescript-angular")</li>
 * </ul>
 */
public final class TypeScriptGroups {
    public static final String TYPESCRIPT = "typescript";
    public static final String TYPESCRIPT_AURELIA = "typescript-aurelia";
    public static final String TYPESCRIPT_AXIOS = "typescript-axios";
    public static final String TYPESCRIPT_FETCH = "typescript-fetch";
    public static final String TYPESCRIPT_ANGULAR = "typescript-angular";
    public static final String TYPESCRIPT_NESTJS = "typescript-nestjs";
    public static final String TYPESCRIPT_NODE = "typescript-node";
}
