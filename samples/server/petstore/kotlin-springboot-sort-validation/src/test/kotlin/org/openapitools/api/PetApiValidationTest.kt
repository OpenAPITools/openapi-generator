package org.openapitools.api

import org.junit.jupiter.api.Test
import org.springframework.beans.factory.annotation.Autowired
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc
import org.springframework.boot.test.context.SpringBootTest
import org.springframework.test.web.servlet.MockMvc
import org.springframework.test.web.servlet.get

/**
 * Verifies the runtime behaviour of the annotations generated onto [PetApi]:
 *
 * - **@ValidSort** — invalid sort field/direction combinations are rejected with 400.
 * - **@ValidPageable** — page number or size that exceeds the configured limit is rejected with 400.
 * - **@PageableDefault** — when page/size query params are absent, the configured defaults are
 *   forwarded to the controller method (verified by assertions inside [PetApiImpl]).
 * - **@SortDefault / @SortDefaults** — when the sort query param is absent, the configured
 *   default sort order is forwarded to the controller method (verified inside [PetApiImpl]).
 *
 * HTTP 200 responses confirm both that the request was accepted *and* that [PetApiImpl]'s
 * internal assertions about the received defaults passed.
 * HTTP 400 responses confirm that the constraint annotation rejected the invalid input.
 */
@SpringBootTest
@AutoConfigureMockMvc
class PetApiValidationTest {

    @Autowired
    lateinit var mockMvc: MockMvc

    // ── @ValidSort ────────────────────────────────────────────────────────────
    // Endpoint: GET /pet/findWithArraySortEnum  allowed: id,asc | id,desc | name,asc | name,desc

    @Test
    fun `ValidSort - valid sort value returns 200`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM}") {
            param("sort", "id,asc")
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `ValidSort - multiple valid sort values return 200`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM}") {
            param("sort", "id,desc")
            param("sort", "name,asc")
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `ValidSort - invalid sort property returns 400`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM}") {
            param("sort", "unknown,asc")
        }.andExpect { status { isBadRequest() } }
    }

    @Test
    fun `ValidSort - invalid sort direction returns 400`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM}") {
            param("sort", "id,random")
        }.andExpect { status { isBadRequest() } }
    }

    @Test
    fun `ValidSort - one invalid sort among multiple valid values returns 400`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM}") {
            param("sort", "id,asc")
            param("sort", "unknown,desc")
        }.andExpect { status { isBadRequest() } }
    }

    // ── @ValidPageable — size constraint only ─────────────────────────────────
    // Endpoint: GET /pet/findWithSizeConstraint  maxSize = 100

    @Test
    fun `ValidPageable - size below maximum returns 200`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT}") {
            param("size", "50")
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `ValidPageable - size at maximum returns 200`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT}") {
            param("size", "100")
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `ValidPageable - size exceeds maximum returns 400`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT}") {
            param("size", "101")
        }.andExpect { status { isBadRequest() } }
    }

    @Test
    fun `ValidPageable - unpaged Pageable is allowed (no params, no PageableDefault)`() {
        // When no pagination parameters are supplied and no @PageableDefault is configured,
        // Spring resolves Pageable.unpaged(). The validator must not throw and must return valid.
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT}")
            .andExpect { status { isOk() } }
    }

    // ── @ValidPageable — size and page constraints combined ───────────────────
    // Endpoint: GET /pet/findWithPageAndSizeConstraint  maxSize = 50, maxPage = 999

    @Test
    fun `ValidPageable - size and page at their maximums return 200`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_PAGE_AND_SIZE_CONSTRAINT}") {
            param("size", "50")
            param("page", "999")
        }.andExpect { status { isOk() } }
    }

    @Test
    fun `ValidPageable - size exceeds maximum for combined constraint returns 400`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_PAGE_AND_SIZE_CONSTRAINT}") {
            param("size", "51")
        }.andExpect { status { isBadRequest() } }
    }

    @Test
    fun `ValidPageable - page exceeds maximum returns 400`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_PAGE_AND_SIZE_CONSTRAINT}") {
            param("page", "1000")
        }.andExpect { status { isBadRequest() } }
    }

    // ── @PageableDefault ─────────────────────────────────────────────────────
    // Endpoint: GET /pet/findWithPageSizeDefaultsOnly  @PageableDefault(page = 0, size = 25)
    // PetApiImpl asserts page == 0 and size == 25; returns 200 on success, throws on mismatch.

    @Test
    fun `PageableDefault - absent params resolve to configured page and size defaults`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_PAGE_SIZE_DEFAULTS_ONLY}")
            .andExpect { status { isOk() } }
    }

    // ── @SortDefault ─────────────────────────────────────────────────────────
    // Endpoint: GET /pet/findWithSortDefaultOnly  @SortDefault(sort = ["name"], direction = DESC)
    // PetApiImpl asserts name DESC; returns 200 on success, throws on mismatch.

    @Test
    fun `SortDefault - absent sort param resolves to name DESC default`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_SORT_DEFAULT_ONLY}")
            .andExpect { status { isOk() } }
    }

    // Endpoint: GET /pet/findWithSortDefaultAsc  @SortDefault(sort = ["id"], direction = ASC)

    @Test
    fun `SortDefault - absent sort param resolves to id ASC default`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_SORT_DEFAULT_ASC}")
            .andExpect { status { isOk() } }
    }

    // ── @SortDefault.SortDefaults ─────────────────────────────────────────────
    // Endpoint: GET /pet/findWithMixedSortDefaults
    // @SortDefaults(SortDefault(["name"], DESC), SortDefault(["id"], ASC))
    // PetApiImpl asserts both orders; returns 200 on success, throws on mismatch.

    @Test
    fun `SortDefaults - absent sort param resolves all configured sort defaults`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_MIXED_SORT_DEFAULTS}")
            .andExpect { status { isOk() } }
    }

    // ── @PageableDefault + @SortDefault.SortDefaults combined ─────────────────
    // Endpoint: GET /pet/findWithAllDefaults
    // @PageableDefault(page = 0, size = 10) + @SortDefaults(name DESC, id ASC)
    // PetApiImpl asserts page, size, and both sort orders.

    @Test
    fun `PageableDefault and SortDefaults combined - absent params resolve all defaults`() {
        mockMvc.get("${PetApi.BASE_PATH}${PetApi.PATH_FIND_PETS_WITH_ALL_DEFAULTS}")
            .andExpect { status { isOk() } }
    }
}
