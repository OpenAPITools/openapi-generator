// CrystalApiRoutingTest.java
package org.openapitools.codegen.crystal;

import org.openapitools.codegen.languages.crystal.CrystalApiRouting;
import org.testng.annotations.Test;
import java.util.*;
import static org.testng.Assert.assertEquals;

public class CrystalApiRoutingTest {

    private static final List<String> NETBOX = Arrays.asList(
        "/api/dcim/cable-terminations/", "/api/dcim/cable-terminations/{id}/",
        "/api/dcim/cable-terminations/{id}/paths/", "/api/ipam/vlans/{id}/");

    private static final List<String> QDRANT = Arrays.asList(
        "/collections", "/collections/{collection_name}",
        "/collections/{collection_name}/points/query",
        "/collections/{collection_name}/points/{id}",
        "/collections/{collection_name}/exists",
        "/cluster/recover", "/cluster/peer/{peer_id}", "/");

    @Test
    public void testResourceSegmentsNetbox() {
        Set<String> r = CrystalApiRouting.resourceSegments(NETBOX);
        assertEquals(r, new HashSet<>(Arrays.asList("cable-terminations", "vlans")));
    }

    @Test
    public void testResourceSegmentsQdrant() {
        Set<String> r = CrystalApiRouting.resourceSegments(QDRANT);
        assertEquals(r, new HashSet<>(Arrays.asList("collections", "points", "peer")));
    }

    @Test
    public void testCommonBasePrefix() {
        assertEquals(CrystalApiRouting.commonBasePrefix(NETBOX), "api");
        assertEquals(CrystalApiRouting.commonBasePrefix(QDRANT), "");
    }

    @Test
    public void testLiterals() {
        assertEquals(CrystalApiRouting.literals("/api/dcim/cable-terminations/{id}/paths/", "api"),
                Arrays.asList("dcim", "cable-terminations", "paths"));
        assertEquals(CrystalApiRouting.literals("/collections/{collection_name}/points/query", ""),
                Arrays.asList("collections", "points", "query"));
        assertEquals(CrystalApiRouting.literals("/", ""), Collections.emptyList());
    }

    @Test
    public void testRouteNetboxItemUsesOperationIdAction() {
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(NETBOX);
        CrystalApiRouting.Route route = CrystalApiRouting.route(
            "/api/dcim/cable-terminations/{id}/", "PATCH",
            "dcim_cable_terminations_partial_update", r, "api");
        assertEquals(route.namespace, "dcim");
        assertEquals(route.resource, "cable-terminations");
        assertEquals(route.action, "partial_update");
    }

    @Test
    public void testRouteQdrantActionSegment() {
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(QDRANT);
        CrystalApiRouting.Route route = CrystalApiRouting.route(
            "/collections/{collection_name}/points/query", "POST", "query_points", r, "");
        assertEquals(route.namespace, "collections");
        assertEquals(route.resource, "points");
        assertEquals(route.action, "query");   // operationId "query_points" doesn't start with group prefix "points_", so branch (b) applies: last literal segment "query"
    }

    @Test
    public void testRouteDegenerateNoResource() {
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(QDRANT);
        CrystalApiRouting.Route route = CrystalApiRouting.route(
            "/cluster/recover", "POST", "recover_cluster", r, "");
        assertEquals(route.namespace, "cluster");
        assertEquals(route.resource, null);
        assertEquals(route.action, "recover");
    }

    @Test
    public void testRouteVerbFallbackCollectionVsItem() {
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(QDRANT);
        // no operationId, no extra literal -> verb CRUD, collection-aware
        CrystalApiRouting.Route coll = CrystalApiRouting.route("/collections", "GET", "", r, "");
        assertEquals(coll.namespace, "collections");
        assertEquals(coll.resource, null);
        assertEquals(coll.action, "list");
        CrystalApiRouting.Route item = CrystalApiRouting.route("/collections/{collection_name}", "GET", "", r, "");
        assertEquals(item.action, "get");
    }

    @Test
    public void testRouteRootPath() {
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(QDRANT);
        CrystalApiRouting.Route route = CrystalApiRouting.route("/", "GET", "", r, "");
        assertEquals(route.namespace, "root");
        assertEquals(route.resource, null);
        // lits is empty for "/", no operationId, not an item path -> verbAction("GET", false) -> "list"
        assertEquals(route.action, "list");
    }

    @Test
    public void testRouteCamelCaseActionName() {
        // /pet/findByStatus → action segment is camelCase; must be snake_cased
        List<String> petPaths = Arrays.asList("/pet", "/pet/{petId}", "/pet/findByStatus", "/pet/findByTags");
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(petPaths);
        // findByStatus is not a resource (it never precedes a {param}), so it becomes an action literal
        CrystalApiRouting.Route route = CrystalApiRouting.route(
            "/pet/findByStatus", "GET", "findPetsByStatus", r, "");
        // operationId "findPetsByStatus" → underscore → "find_pets_by_status"
        // group = "pet" (no resource since findByStatus ∉ r), group_ = "pet_"
        // "find_pets_by_status" does not start with "pet_" ... so falls to (b): action literal = findByStatus → "find_by_status"
        assertEquals(route.namespace, "pet");
        assertEquals(route.resource, null);
        assertEquals(route.action, "find_by_status");
    }

    @Test
    public void testRouteCamelCaseOperationId() {
        // operationId with camelCase prefix strip: e.g. "petFindByStatus" from group "pet"
        List<String> petPaths = Arrays.asList("/pet", "/pet/{petId}", "/pet/findByStatus");
        java.util.Set<String> r = CrystalApiRouting.resourceSegments(petPaths);
        CrystalApiRouting.Route route = CrystalApiRouting.route(
            "/pet/findByStatus", "GET", "petFindByStatus", r, "");
        // operationId "petFindByStatus" → underscore → "pet_find_by_status"
        // group = "pet", g = "pet_"
        // "pet_find_by_status".startsWith("pet_") and rest = "find_by_status"
        assertEquals(route.namespace, "pet");
        assertEquals(route.resource, null);
        assertEquals(route.action, "find_by_status");
    }
}
