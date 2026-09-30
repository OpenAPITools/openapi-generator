package org.openapitools.api

import org.openapitools.model.Pet
import org.springframework.data.domain.Pageable
import org.springframework.data.domain.Sort
import org.springframework.http.ResponseEntity
import org.springframework.stereotype.Component
import org.springframework.web.bind.annotation.RestController

/**
 * Sample implementation of [PetApi] demonstrating that the generated
 * annotations (@ValidSort, @ValidPageable, @PageableDefault, @SortDefault)
 * behave correctly at runtime.
 *
 * Methods whose endpoint carries pagination/sort defaults assert the exact
 * expected values inside the method body. When the Spring argument resolvers
 * apply the annotated defaults correctly, the assertions pass and HTTP 200 is
 * returned. If a default is missing or wrong, the assertion throws
 * [IllegalStateException] and the request fails with HTTP 500, which causes
 * any calling test to fail with a clear message.
 *
 * Methods that only carry @ValidSort / @ValidPageable constraints need no body
 * logic — the constraint annotations reject invalid input before this code is
 * ever reached, and the [DefaultExceptionHandler] maps the resulting
 * [jakarta.validation.ConstraintViolationException] to HTTP 400.
 */
@RestController
class PetApiImpl : PetApi {

    // ── no pageable / no special defaults ────────────────────────────────────

    override fun findPetsAutoDetectedWithSort(
        status: String?,
        page: Int,
        size: Int,
        sort: String?,
    ): ResponseEntity<List<Pet>> = ResponseEntity.ok(emptyList())

    override fun findPetsNonPaginatedWithSortEnum(
        sort: String?,
    ): ResponseEntity<List<Pet>> = ResponseEntity.ok(emptyList())

    // ── @ValidSort only (+ @PageableDefault) ─────────────────────────────────
    // Validation rejects bad sort values before the method is called.

    override fun findPetsWithArraySortEnum(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithArraySortRefEnum(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithExternalParamRefArraySort(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithNonExplodedExternalParamRefArraySort(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithRefSort(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithSortEnum(status: String?, pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithoutSortEnum(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    // ── @ValidPageable only ───────────────────────────────────────────────────
    // Validation rejects out-of-range page / size before the method is called.

    override fun findPetsWithSizeConstraint(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    override fun findPetsWithPageAndSizeConstraint(pageable: Pageable): ResponseEntity<List<Pet>> =
        ResponseEntity.ok(emptyList())

    // ── @PageableDefault ─────────────────────────────────────────────────────
    // @PageableDefault(page = 0, size = 25)

    override fun findPetsWithPageSizeDefaultsOnly(pageable: Pageable): ResponseEntity<List<Pet>> {
        check(pageable.pageNumber == 0) { "@PageableDefault page: expected 0, got ${pageable.pageNumber}" }
        check(pageable.pageSize == 25) { "@PageableDefault size: expected 25, got ${pageable.pageSize}" }
        return ResponseEntity.ok(emptyList())
    }

    // ── @SortDefault ─────────────────────────────────────────────────────────
    // @SortDefault(sort = ["name"], direction = DESC)

    override fun findPetsWithSortDefaultOnly(pageable: Pageable): ResponseEntity<List<Pet>> {
        check(pageable.sort.getOrderFor("name")?.direction == Sort.Direction.DESC) {
            "@SortDefault sort: expected name DESC, got ${pageable.sort}"
        }
        return ResponseEntity.ok(emptyList())
    }

    // @SortDefault(sort = ["id"], direction = ASC)

    override fun findPetsWithSortDefaultAsc(pageable: Pageable): ResponseEntity<List<Pet>> {
        check(pageable.sort.getOrderFor("id")?.direction == Sort.Direction.ASC) {
            "@SortDefault sort: expected id ASC, got ${pageable.sort}"
        }
        return ResponseEntity.ok(emptyList())
    }

    // ── @SortDefault.SortDefaults ─────────────────────────────────────────────
    // @SortDefaults(SortDefault(sort = ["name"], direction = DESC), SortDefault(sort = ["id"], direction = ASC))

    override fun findPetsWithMixedSortDefaults(pageable: Pageable): ResponseEntity<List<Pet>> {
        check(pageable.sort.getOrderFor("name")?.direction == Sort.Direction.DESC) {
            "@SortDefaults sort: expected name DESC, got ${pageable.sort}"
        }
        check(pageable.sort.getOrderFor("id")?.direction == Sort.Direction.ASC) {
            "@SortDefaults sort: expected id ASC, got ${pageable.sort}"
        }
        return ResponseEntity.ok(emptyList())
    }

    // ── @PageableDefault + @SortDefault.SortDefaults combined ─────────────────
    // @PageableDefault(page = 0, size = 10)
    // @SortDefaults(SortDefault(sort = ["name"], direction = DESC), SortDefault(sort = ["id"], direction = ASC))

    override fun findPetsWithAllDefaults(pageable: Pageable): ResponseEntity<List<Pet>> {
        check(pageable.pageNumber == 0) { "@PageableDefault page: expected 0, got ${pageable.pageNumber}" }
        check(pageable.pageSize == 10) { "@PageableDefault size: expected 10, got ${pageable.pageSize}" }
        check(pageable.sort.getOrderFor("name")?.direction == Sort.Direction.DESC) {
            "@SortDefaults sort: expected name DESC, got ${pageable.sort}"
        }
        check(pageable.sort.getOrderFor("id")?.direction == Sort.Direction.ASC) {
            "@SortDefaults sort: expected id ASC, got ${pageable.sort}"
        }
        return ResponseEntity.ok(emptyList())
    }
}
