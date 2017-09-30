package io.swagger.codegen.utils;

public class SemVer implements Comparable<SemVer> {

    public final int major;
    public final int minor;
    public final int revision;
    
    public SemVer(String versionString) {
        String[] tokens = versionString.split("\\.");
        major = Integer.parseInt(tokens[0]);
        minor = tokens.length < 2 ? 0 : Integer.parseInt(tokens[1]);
        revision = tokens.length < 3 ? 0 : Integer.parseInt(tokens[2]);
    }
    
    @Override
    public int compareTo(SemVer o) {
        int cmp = major - o.major;
        if (cmp != 0) return cmp;
        cmp = minor - o.minor;
        if (cmp != 0) return cmp;
        return revision - o.revision;
    }
    
    public boolean atLeast(String other) {
        return compareTo(new SemVer(other)) >= 0;
    }
    
    @Override
    public String toString() {
        return major + "." + minor + "." + revision;
    }
}