// RubyApiRouting.java
package org.openapitools.codegen.languages.rubynextgen;

import java.util.*;
import org.openapitools.codegen.utils.StringUtils;

// NOTE: Intentional copy of CrystalApiRouting. The path->route logic is
// language-agnostic; it is duplicated (not shared) to keep each generator
// fully decoupled and avoid modifying the already-shipped Crystal generator.
/** Pure routing logic for the ruby-nextgengenerator: path -> (namespace, resource, action). */
public final class RubyApiRouting {
    private RubyApiRouting() {}

    private static boolean isParam(String seg) { return seg.startsWith("{") && seg.endsWith("}"); }

    private static List<String> segments(String path) {
        List<String> out = new ArrayList<>();
        for (String s : path.split("/")) {
            if (!s.isEmpty()) out.add(s);
        }
        return out;
    }

    /** Segments that appear immediately before a {param} in at least one path. */
    public static Set<String> resourceSegments(Collection<String> paths) {
        Set<String> r = new HashSet<>();
        for (String path : paths) {
            List<String> segs = segments(path);
            for (int i = 0; i < segs.size(); i++) {
                if (!isParam(segs.get(i)) && i + 1 < segs.size() && isParam(segs.get(i + 1))) {
                    r.add(segs.get(i));
                }
            }
        }
        return r;
    }

    /** Longest leading run of identical literal segments shared by all paths. */
    public static String commonBasePrefix(Collection<String> paths) {
        List<List<String>> all = new ArrayList<>();
        for (String p : paths) all.add(segments(p));
        StringBuilder prefix = new StringBuilder();
        for (int i = 0; ; i++) {
            String candidate = null;
            for (List<String> segs : all) {
                if (i >= segs.size() || isParam(segs.get(i))) return prefix.toString();
                if (candidate == null) candidate = segs.get(i);
                else if (!candidate.equals(segs.get(i))) return prefix.toString();
            }
            if (candidate == null) return prefix.toString();
            // only treat as base prefix if every path has more segments after it
            for (List<String> segs : all) if (segs.size() <= i + 1) return prefix.toString();
            if (prefix.length() > 0) prefix.append("/");
            prefix.append(candidate);
        }
    }

    /** Literal segments after stripping the base prefix and dropping {param} segments. */
    public static List<String> literals(String path, String basePrefix) {
        List<String> segs = segments(path);
        List<String> prefix = basePrefix.isEmpty() ? Collections.emptyList() : segments(basePrefix);
        int start = 0;
        if (!prefix.isEmpty() && segs.size() >= prefix.size()
                && segs.subList(0, prefix.size()).equals(prefix)) {
            start = prefix.size();
        }
        List<String> out = new ArrayList<>();
        for (int i = start; i < segs.size(); i++) {
            if (!isParam(segs.get(i))) out.add(segs.get(i));
        }
        return out;
    }

    /** Route triple: (namespace, resource, action). Resource may be null. */
    public static final class Route {
        public final String namespace;
        public final String resource;   // nullable
        public final String action;
        public Route(String namespace, String resource, String action) {
            this.namespace = namespace;
            this.resource = resource;
            this.action = action;
        }
    }

    private static String underscore(String s) {
        return StringUtils.underscore(s);
    }

    public static Route route(String path, String httpMethod, String operationId,
                              Set<String> rset, String basePrefix) {
        List<String> lits = literals(path, basePrefix);
        if (lits.isEmpty()) {
            return new Route("root", null, verbAction(httpMethod, endsWithParam(path)));
        }
        String namespace = lits.get(0);
        String resource = null;
        int actionStart = 1;
        if (lits.size() >= 2 && rset.contains(lits.get(1))) {
            resource = lits.get(1);
            actionStart = 2;
        }
        String group = resource == null ? namespace : namespace + "_" + resource;
        String action = deriveAction(lits, actionStart, group, httpMethod, operationId, endsWithParam(path));
        return new Route(namespace, resource, action);
    }

    private static boolean endsWithParam(String path) {
        List<String> segs = segments(path);
        return !segs.isEmpty() && isParam(segs.get(segs.size() - 1));
    }

    private static String deriveAction(List<String> lits, int actionStart, String group,
                                       String httpMethod, String operationId, boolean item) {
        // (a) operationId minus the group prefix
        if (operationId != null && !operationId.isEmpty()) {
            String op = underscore(operationId);
            String g = underscore(group) + "_";
            if (op.startsWith(g) && op.length() > g.length()) {
                return op.substring(g.length());
            }
        }
        // (b) remaining literal action segments
        if (actionStart < lits.size()) {
            StringBuilder sb = new StringBuilder();
            for (int i = actionStart; i < lits.size(); i++) {
                if (sb.length() > 0) sb.append("_");
                sb.append(underscore(lits.get(i)));
            }
            return sb.toString();
        }
        // (c) collection-aware verb CRUD
        return verbAction(httpMethod, item);
    }

    private static String verbAction(String httpMethod, boolean item) {
        switch (httpMethod.toUpperCase(java.util.Locale.ROOT)) {
            case "GET":    return item ? "get" : "list";
            case "POST":   return "create";
            case "PUT":    return item ? "update" : "bulk_update";
            case "PATCH":  return item ? "partial_update" : "bulk_partial_update";
            case "DELETE": return item ? "delete" : "bulk_destroy";
            default:       return httpMethod.toLowerCase(java.util.Locale.ROOT);
        }
    }
}
