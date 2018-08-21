package org.openapitools.codegen;

import java.util.*;

public class CodegenCallback {
    public String name;
    public boolean hasMore;
    public List<Url> urls = new ArrayList<>();
    public Map<String, Object> vendorExtensions = new HashMap<>();

    public static class Url {
        public String expression;
        public boolean hasMore;
        public List<CodegenOperation> requests = new ArrayList<>();
        public Map<String, Object> vendorExtensions = new HashMap<>();

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Url that = (Url) o;
            return Objects.equals(that.expression, expression) && Objects.equals(that.hasMore, hasMore) &&
                    Objects.equals(that.requests, requests) && Objects.equals(that.vendorExtensions, vendorExtensions);
        }
        @Override
        public int hashCode() {
            return Objects.hash(expression, hasMore, requests, vendorExtensions);
        }
        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append("CodegenCallback.Urls {\n");
            sb.append("    expression: ").append(expression).append("\n");
            requests.forEach(r -> sb.append("   ").append(r).append("\n"));
            sb.append("}");
            return sb.toString();
        }
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenCallback that = (CodegenCallback) o;
        return Objects.equals(that.name, name) && Objects.equals(that.hasMore, hasMore) &&
                Objects.equals(that.urls, urls) && Objects.equals(that.vendorExtensions, vendorExtensions);
    }
    @Override
    public int hashCode() {
        return Objects.hash(name, hasMore, urls, vendorExtensions);
    }
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CodegenCallback {\n");
        sb.append("    name: ").append(name).append("\n");
        urls.forEach(u -> sb.append("   ").append(u).append("\n"));
        sb.append("}");
        return sb.toString();
    }
}
