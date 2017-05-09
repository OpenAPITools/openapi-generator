package io.swagger.codegen.utils;

public class ImplementationVersion {
    public static String read() {
        // Assumes this version is required at runtime. This could be modified to use a properties file like the CLI.
        String compiledVersion = ImplementationVersion.class.getPackage().getImplementationVersion();
        if(compiledVersion != null) {
            return compiledVersion;
        }

        return "unset";
    }
}
