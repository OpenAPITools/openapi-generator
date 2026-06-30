package org.openapitools.codegen.utils;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.TestUtils;
import org.testng.annotations.Test;

import java.util.Optional;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;

public class DiscriminatorUtilsTest {

    /**
     * A pure oneOf interface schema declares no properties of its own, so resolving the discriminator
     * property type from the schema alone yields nothing. The type must instead be resolved from the
     * mapped child schemas, which inherit the discriminator property from a shared base via allOf.
     */
    @Test
    public void resolvesDiscriminatorPropertyTypeFromChildrenWhenNotOnSchema() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneof_discriminator_enum_shared_base.yaml");
        Schema petRequest = openAPI.getComponents().getSchemas().get("PetRequest");

        // The oneOf interface declares no discriminator property of its own, so the type is resolved from
        // the mapped children, finding the enum ref (PetType) declared on the shared base.
        Optional<String> resolved = DiscriminatorUtils.getDiscriminatorPropertyType(openAPI, petRequest, "petType");
        assertEquals(resolved.orElse(null), "PetType");
    }

    /**
     * When the discriminator property is declared directly on the schema, the own-properties resolution is
     * used and the children are not consulted.
     */
    @Test
    public void resolvesDiscriminatorPropertyTypeFromOwnPropertiesWhenPresent() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneof_discriminator_enum_shared_base.yaml");
        Schema petBase = openAPI.getComponents().getSchemas().get("PetBase");

        Optional<String> resolved = DiscriminatorUtils.getDiscriminatorPropertyType(openAPI, petBase, "petType");
        assertEquals(resolved.orElse(null), "PetType");
    }

    /**
     * The children fallback also resolves the discriminator property type when the interface uses anyOf
     * rather than oneOf.
     */
    @Test
    public void resolvesDiscriminatorPropertyTypeFromAnyOfChildren() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/anyof_discriminator_enum_shared_base.yaml");
        Schema petRequest = openAPI.getComponents().getSchemas().get("PetRequest");

        Optional<String> resolved = DiscriminatorUtils.getDiscriminatorPropertyType(openAPI, petRequest, "petType");
        assertEquals(resolved.orElse(null), "PetType");
    }

    /**
     * The discriminator property is reached only by recursing through more than one level of allOf (the
     * children allOf an intermediate base that itself allOf the grandparent declaring the property).
     */
    @Test
    public void resolvesDiscriminatorPropertyTypeThroughMultiLevelAllOf() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneof_discriminator_enum_nested_base.yaml");
        Schema petRequest = openAPI.getComponents().getSchemas().get("PetRequest");

        Optional<String> resolved = DiscriminatorUtils.getDiscriminatorPropertyType(openAPI, petRequest, "petType");
        assertEquals(resolved.orElse(null), "PetType");
    }

    /**
     * When neither the schema nor its children declare the discriminator property as a typed ($ref) schema
     * - the children carry it only as a plain inline string - there is nothing to resolve, so the lookup
     * returns empty (the caller then falls back to "string").
     */
    @Test
    public void returnsEmptyWhenDiscriminatorPropertyHasNoTypedSchema() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneof_discriminator_string_fallback.yaml");
        Schema petRequest = openAPI.getComponents().getSchemas().get("PetRequest");

        assertFalse(DiscriminatorUtils.getDiscriminatorPropertyType(openAPI, petRequest, "petType").isPresent());
    }

    /**
     * A cyclic allOf composition (two bases that allOf each other) must not cause the allOf descent to
     * recurse infinitely. Resolution terminates; since no schema in the cycle declares the discriminator
     * property as a typed $ref, the lookup returns empty.
     */
    @Test
    public void terminatesOnCyclicAllOfComposition() {
        final OpenAPI openAPI = TestUtils.parseFlattenSpec("src/test/resources/3_0/oneof_discriminator_cyclic_allof.yaml");
        Schema petRequest = openAPI.getComponents().getSchemas().get("PetRequest");

        // Must return (not StackOverflowError) - the visited-set guard breaks the allOf cycle.
        assertFalse(DiscriminatorUtils.getDiscriminatorPropertyType(openAPI, petRequest, "petType").isPresent());
    }
}
