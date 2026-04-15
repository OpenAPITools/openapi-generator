package org.openapitools.api;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Verifies the runtime behaviour of the annotations generated onto {@link PetApi}:
 *
 * <ul>
 *   <li>{@code @ValidSort} — invalid sort field/direction combinations are rejected with 400.</li>
 *   <li>{@code @ValidPageable} — page number or size exceeding the configured limit is rejected with 400.</li>
 *   <li>{@code @PageableDefault} — when page/size query params are absent, the configured defaults are
 *       forwarded to the controller method (verified by assertions inside {@link PetApiController}).</li>
 *   <li>{@code @SortDefault} / {@code @SortDefaults} — when the sort query param is absent, the configured
 *       default sort order is forwarded to the controller method (verified inside {@link PetApiController}).</li>
 * </ul>
 *
 * HTTP 200 responses confirm both that the request was accepted <em>and</em> that {@link PetApiController}'s
 * internal assertions about the received defaults passed.
 * HTTP 400 responses confirm that the constraint annotation rejected the invalid input.
 */
@SpringBootTest
@AutoConfigureMockMvc
class PetApiValidationTest {

    @Autowired
    MockMvc mockMvc;

    // ── @ValidSort ────────────────────────────────────────────────────────────
    // Endpoint: GET /pet/findWithArraySortEnum  allowed: id,asc | id,desc | name,asc | name,desc

    @Test
    void validSort_validSortValueReturns200() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM)
                        .param("sort", "id,asc"))
                .andExpect(status().isOk());
    }

    @Test
    void validSort_multipleValidSortValuesReturn200() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM)
                        .param("sort", "id,desc")
                        .param("sort", "name,asc"))
                .andExpect(status().isOk());
    }

    @Test
    void validSort_invalidSortPropertyReturns400() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM)
                        .param("sort", "unknown,asc"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void validSort_invalidSortDirectionReturns400() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM)
                        .param("sort", "id,random"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void validSort_oneInvalidSortAmongMultipleValidValuesReturns400() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_ARRAY_SORT_ENUM)
                        .param("sort", "id,asc")
                        .param("sort", "unknown,desc"))
                .andExpect(status().isBadRequest());
    }

    // ── @ValidPageable — size constraint only ─────────────────────────────────
    // Endpoint: GET /pet/findWithSizeConstraint  maxSize = 100

    @Test
    void validPageable_sizeBelowMaximumReturns200() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT)
                        .param("size", "50"))
                .andExpect(status().isOk());
    }

    @Test
    void validPageable_sizeAtMaximumReturns200() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT)
                        .param("size", "100"))
                .andExpect(status().isOk());
    }

    @Test
    void validPageable_sizeExceedsMaximumReturns400() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT)
                        .param("size", "101"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void validPageable_unpagedPageableIsAllowed() throws Exception {
        // When no pagination parameters are supplied and no @PageableDefault is configured,
        // Spring resolves Pageable.unpaged(). The validator must not throw and must return valid.
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_SIZE_CONSTRAINT))
                .andExpect(status().isOk());
    }

    // ── @ValidPageable — size and page constraints combined ───────────────────
    // Endpoint: GET /pet/findWithPageAndSizeConstraint  maxSize = 50, maxPage = 999

    @Test
    void validPageable_sizeAndPageAtTheirMaximumsReturn200() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_PAGE_AND_SIZE_CONSTRAINT)
                        .param("size", "50")
                        .param("page", "999"))
                .andExpect(status().isOk());
    }

    @Test
    void validPageable_sizeExceedsMaximumForCombinedConstraintReturns400() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_PAGE_AND_SIZE_CONSTRAINT)
                        .param("size", "51"))
                .andExpect(status().isBadRequest());
    }

    @Test
    void validPageable_pageExceedsMaximumReturns400() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_PAGE_AND_SIZE_CONSTRAINT)
                        .param("page", "1000"))
                .andExpect(status().isBadRequest());
    }

    // ── @PageableDefault ─────────────────────────────────────────────────────
    // Endpoint: GET /pet/findWithPageSizeDefaultsOnly  @PageableDefault(page = 0, size = 25)
    // PetApiController asserts page == 0 and size == 25; returns 200 on success, throws on mismatch.

    @Test
    void pageableDefault_absentParamsResolveToConfiguredPageAndSizeDefaults() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_PAGE_SIZE_DEFAULTS_ONLY))
                .andExpect(status().isOk());
    }

    // ── @SortDefault ─────────────────────────────────────────────────────────
    // Endpoint: GET /pet/findWithSortDefaultOnly  @SortDefault(sort = {"name"}, direction = DESC)
    // PetApiController asserts name DESC; returns 200 on success, throws on mismatch.

    @Test
    void sortDefault_absentSortParamResolvesToNameDescDefault() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_SORT_DEFAULT_ONLY))
                .andExpect(status().isOk());
    }

    // Endpoint: GET /pet/findWithSortDefaultAsc  @SortDefault(sort = {"id"}, direction = ASC)

    @Test
    void sortDefault_absentSortParamResolvesToIdAscDefault() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_SORT_DEFAULT_ASC))
                .andExpect(status().isOk());
    }

    // ── @SortDefault.SortDefaults ─────────────────────────────────────────────
    // Endpoint: GET /pet/findWithMixedSortDefaults
    // @SortDefaults(SortDefault({"name"}, DESC), SortDefault({"id"}, ASC))
    // PetApiController asserts both orders; returns 200 on success, throws on mismatch.

    @Test
    void sortDefaults_absentSortParamResolvesAllConfiguredSortDefaults() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_MIXED_SORT_DEFAULTS))
                .andExpect(status().isOk());
    }

    // ── @PageableDefault + @SortDefault.SortDefaults combined ─────────────────
    // Endpoint: GET /pet/findWithAllDefaults
    // @PageableDefault(page = 0, size = 10) + @SortDefaults(name DESC, id ASC)
    // PetApiController asserts page, size, and both sort orders.

    @Test
    void pageableDefaultAndSortDefaults_absentParamsResolveAllDefaults() throws Exception {
        mockMvc.perform(get(PetApi.PATH_FIND_PETS_WITH_ALL_DEFAULTS))
                .andExpect(status().isOk());
    }
}
