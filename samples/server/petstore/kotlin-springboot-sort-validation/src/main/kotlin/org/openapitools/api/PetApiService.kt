package org.openapitools.api

import org.springframework.data.domain.Pageable
import org.springframework.data.web.PageableDefault
import org.openapitools.model.Pet
import org.openapitools.model.PetSort
import org.openapitools.model.PetSortEnum
import org.springframework.data.domain.Sort
import org.springframework.data.web.SortDefault
import org.openapitools.configuration.ValidPageable
import org.openapitools.configuration.ValidSort

interface PetApiService {

    /**
     * GET /pet/findAutoDetectedWithSort : Find pets with auto-detected pagination and sort enum
     *
     * @param status Status filter (optional)
     * @param page  (optional, default to 0)
     * @param size  (optional, default to 20)
     * @param sort Sort order (optional)
     * @return successful operation (status code 200)
     * @see PetApi#findPetsAutoDetectedWithSort
     */
    fun findPetsAutoDetectedWithSort(status: kotlin.String?, page: kotlin.Int, size: kotlin.Int, sort: kotlin.String?): List<Pet>

    /**
     * GET /pet/findNonPaginatedWithSortEnum : Find pets without pagination but sort param has enum — no sort validation expected
     *
     * @param sort Sort order with enum but no pagination (optional)
     * @return successful operation (status code 200)
     * @see PetApi#findPetsNonPaginatedWithSortEnum
     */
    fun findPetsNonPaginatedWithSortEnum(sort: kotlin.String?): List<Pet>

    /**
     * GET /pet/findWithAllDefaults : Find pets — page, size, and mixed sort defaults all present
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithAllDefaults
     */
    fun findPetsWithAllDefaults(): List<Pet>

    /**
     * GET /pet/findWithArraySortEnum : Find pets with x-spring-paginated and array sort param with inline enum on items
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithArraySortEnum
     */
    fun findPetsWithArraySortEnum(): List<Pet>

    /**
     * GET /pet/findWithArraySortRefEnum : Find pets with x-spring-paginated and array sort param whose items use a $ref enum
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithArraySortRefEnum
     */
    fun findPetsWithArraySortRefEnum(): List<Pet>

    /**
     * GET /pet/findWithExternalParamRefArraySort : Find pets with x-spring-paginated and sort param referenced from an external components file
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithExternalParamRefArraySort
     */
    fun findPetsWithExternalParamRefArraySort(): List<Pet>

    /**
     * GET /pet/findWithMixedSortDefaults : Find pets — multiple sort defaults with mixed directions (array sort param)
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithMixedSortDefaults
     */
    fun findPetsWithMixedSortDefaults(): List<Pet>

    /**
     * GET /pet/findWithNonExplodedExternalParamRefArraySort : Find pets with x-spring-paginated and non-exploded sort param referenced from an external components file
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithNonExplodedExternalParamRefArraySort
     */
    fun findPetsWithNonExplodedExternalParamRefArraySort(): List<Pet>

    /**
     * GET /pet/findWithPageAndSizeConstraint : Find pets — both page and size have maximum constraints
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithPageAndSizeConstraint
     */
    fun findPetsWithPageAndSizeConstraint(): List<Pet>

    /**
     * GET /pet/findWithPageSizeDefaultsOnly : Find pets — page and size defaults only, no sort default
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithPageSizeDefaultsOnly
     */
    fun findPetsWithPageSizeDefaultsOnly(): List<Pet>

    /**
     * GET /pet/findWithRefSort : Find pets with x-spring-paginated and $ref sort enum
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithRefSort
     */
    fun findPetsWithRefSort(): List<Pet>

    /**
     * GET /pet/findWithSizeConstraint : Find pets — size has maximum constraint only
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithSizeConstraint
     */
    fun findPetsWithSizeConstraint(): List<Pet>

    /**
     * GET /pet/findWithSortDefaultAsc : Find pets — sort default only (single field, no explicit direction defaults to ASC)
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithSortDefaultAsc
     */
    fun findPetsWithSortDefaultAsc(): List<Pet>

    /**
     * GET /pet/findWithSortDefaultOnly : Find pets — sort default only (single field DESC, no page/size defaults)
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithSortDefaultOnly
     */
    fun findPetsWithSortDefaultOnly(): List<Pet>

    /**
     * GET /pet/findByStatusWithSort : Find pets with explicit x-spring-paginated and inline sort enum
     *
     * @param status Status filter (optional)
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithSortEnum
     */
    fun findPetsWithSortEnum(status: kotlin.String?): List<Pet>

    /**
     * GET /pet/findWithoutSortEnum : Find pets with pagination but sort has no enum constraint
     *
     * @return successful operation (status code 200)
     * @see PetApi#findPetsWithoutSortEnum
     */
    fun findPetsWithoutSortEnum(): List<Pet>
}
