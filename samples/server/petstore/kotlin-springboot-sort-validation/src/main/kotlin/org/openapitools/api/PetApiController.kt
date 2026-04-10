package org.openapitools.api

import org.springframework.data.domain.Pageable
import org.openapitools.model.Pet
import org.openapitools.model.PetSort
import org.openapitools.configuration.ValidSort
import org.springframework.http.HttpStatus
import org.springframework.http.MediaType
import org.springframework.http.ResponseEntity

import org.springframework.web.bind.annotation.*
import org.springframework.validation.annotation.Validated
import org.springframework.web.context.request.NativeWebRequest
import org.springframework.beans.factory.annotation.Autowired

import jakarta.validation.Valid
import jakarta.validation.constraints.DecimalMax
import jakarta.validation.constraints.DecimalMin
import jakarta.validation.constraints.Email
import jakarta.validation.constraints.Max
import jakarta.validation.constraints.Min
import jakarta.validation.constraints.NotNull
import jakarta.validation.constraints.Pattern
import jakarta.validation.constraints.Size

import kotlin.collections.List
import kotlin.collections.Map

@RestController
@Validated
class PetApiController(@Autowired(required = true) val service: PetApiService) {


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findAutoDetectedWithSort"
        value = [PATH_FIND_PETS_AUTO_DETECTED_WITH_SORT],
        produces = ["application/json"]
    )
    fun findPetsAutoDetectedWithSort(
        @Valid @RequestParam(value = "status", required = false) status: kotlin.String?,
        @Valid @RequestParam(value = "page", required = false, defaultValue = "0") page: kotlin.Int,
        @Valid @RequestParam(value = "size", required = false, defaultValue = "20") size: kotlin.Int,
        @Valid @RequestParam(value = "sort", required = false) sort: kotlin.String?
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsAutoDetectedWithSort(status, page, size, sort), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findNonPaginatedWithSortEnum"
        value = [PATH_FIND_PETS_NON_PAGINATED_WITH_SORT_ENUM],
        produces = ["application/json"]
    )
    fun findPetsNonPaginatedWithSortEnum(
        @Valid @RequestParam(value = "sort", required = false) sort: kotlin.String?
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsNonPaginatedWithSortEnum(sort), HttpStatus.valueOf(200))
    }


    @ValidSort(allowedValues = ["id,asc", "id,desc", "createdAt,asc", "createdAt,desc"])
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findWithRefSort"
        value = [PATH_FIND_PETS_WITH_REF_SORT],
        produces = ["application/json"]
    )
    fun findPetsWithRefSort(pageable: Pageable): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsWithRefSort(), HttpStatus.valueOf(200))
    }


    @ValidSort(allowedValues = ["id,asc", "id,desc", "name,asc", "name,desc"])
    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findByStatusWithSort"
        value = [PATH_FIND_PETS_WITH_SORT_ENUM],
        produces = ["application/json"]
    )
    fun findPetsWithSortEnum(
        @Valid @RequestParam(value = "status", required = false) status: kotlin.String?,
        pageable: Pageable
    ): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsWithSortEnum(status), HttpStatus.valueOf(200))
    }


    @RequestMapping(
        method = [RequestMethod.GET],
        // "/pet/findWithoutSortEnum"
        value = [PATH_FIND_PETS_WITHOUT_SORT_ENUM],
        produces = ["application/json"]
    )
    fun findPetsWithoutSortEnum(pageable: Pageable): ResponseEntity<List<Pet>> {
        return ResponseEntity(service.findPetsWithoutSortEnum(), HttpStatus.valueOf(200))
    }

    companion object {
        //for your own safety never directly reuse these path definitions in tests
        const val PATH_FIND_PETS_AUTO_DETECTED_WITH_SORT: String = "/pet/findAutoDetectedWithSort"
        const val PATH_FIND_PETS_NON_PAGINATED_WITH_SORT_ENUM: String = "/pet/findNonPaginatedWithSortEnum"
        const val PATH_FIND_PETS_WITH_REF_SORT: String = "/pet/findWithRefSort"
        const val PATH_FIND_PETS_WITH_SORT_ENUM: String = "/pet/findByStatusWithSort"
        const val PATH_FIND_PETS_WITHOUT_SORT_ENUM: String = "/pet/findWithoutSortEnum"
    }
}
