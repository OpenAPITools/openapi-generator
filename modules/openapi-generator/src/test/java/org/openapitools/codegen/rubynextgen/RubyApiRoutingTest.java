/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.rubynextgen;

import org.openapitools.codegen.languages.rubynextgen.RubyApiRouting;
import org.testng.annotations.Test;

import java.util.*;

import static org.testng.Assert.assertEquals;

public class RubyApiRoutingTest {
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
    public void testResourceSegments() {
        assertEquals(RubyApiRouting.resourceSegments(NETBOX),
                new HashSet<>(Arrays.asList("cable-terminations", "vlans")));
        assertEquals(RubyApiRouting.resourceSegments(QDRANT),
                new HashSet<>(Arrays.asList("collections", "points", "peer")));
    }

    @Test
    public void testCommonBasePrefix() {
        assertEquals(RubyApiRouting.commonBasePrefix(NETBOX), "api");
        assertEquals(RubyApiRouting.commonBasePrefix(QDRANT), "");
    }

    @Test
    public void testLiterals() {
        assertEquals(RubyApiRouting.literals("/api/dcim/cable-terminations/{id}/paths/", "api"),
                Arrays.asList("dcim", "cable-terminations", "paths"));
        assertEquals(RubyApiRouting.literals("/", ""), Collections.emptyList());
    }

    @Test
    public void testRouteItemUsesOperationIdAction() {
        Set<String> r = RubyApiRouting.resourceSegments(NETBOX);
        RubyApiRouting.Route route = RubyApiRouting.route(
                "/api/dcim/cable-terminations/{id}/", "PATCH",
                "dcim_cable_terminations_partial_update", r, "api");
        assertEquals(route.namespace, "dcim");
        assertEquals(route.resource, "cable-terminations");
        assertEquals(route.action, "partial_update");
    }

    @Test
    public void testRouteVerbFallbackCollectionVsItem() {
        Set<String> r = RubyApiRouting.resourceSegments(QDRANT);
        assertEquals(RubyApiRouting.route("/collections", "GET", "", r, "").action, "list");
        assertEquals(RubyApiRouting.route("/collections/{collection_name}", "GET", "", r, "").action, "get");
    }

    @Test
    public void testRouteRootPath() {
        Set<String> r = RubyApiRouting.resourceSegments(QDRANT);
        RubyApiRouting.Route route = RubyApiRouting.route("/", "GET", "", r, "");
        assertEquals(route.namespace, "root");
        assertEquals(route.resource, null);
        assertEquals(route.action, "list");
    }
}
