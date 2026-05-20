package org.openapitools.api;

import org.openapitools.model.Pet;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.List;

/**
 * Sample implementation of {@link PetApi} demonstrating that the generated
 * annotations ({@code @ValidSort}, {@code @ValidPageable}, {@code @PageableDefault},
 * {@code @SortDefault}) behave correctly at runtime.
 *
 * Methods whose endpoint carries pagination/sort defaults assert the exact expected values
 * inside the method body. When the Spring argument resolvers apply the annotated defaults
 * correctly, the assertions pass and HTTP 200 is returned. If a default is missing or wrong,
 * the assertion throws {@link IllegalStateException} and the request fails with HTTP 500,
 * causing any calling test to fail with a clear message.
 *
 * Methods that only carry {@code @ValidSort} / {@code @ValidPageable} constraints need no body
 * logic — the constraint annotations reject invalid input before this code is ever reached,
 * and the {@code DefaultExceptionHandler} maps the resulting
 * {@link jakarta.validation.ConstraintViolationException} to HTTP 400.
 */
@RestController
public class PetApiController implements PetApi {

    // ── no pageable / no special defaults ────────────────────────────────────

    @Override
    public ResponseEntity<List<Pet>> findPetsAutoDetectedWithSort(
            String status, Integer page, Integer size, String sort) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsNonPaginatedWithSortEnum(String sort) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    // ── @ValidSort only (+ @PageableDefault) ─────────────────────────────────

    @Override
    public ResponseEntity<List<Pet>> findPetsWithArraySortEnum(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithArraySortRefEnum(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithExternalParamRefArraySort(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithNonExplodedExternalParamRefArraySort(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithRefSort(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithSortEnum(String status, Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithoutSortEnum(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    // ── @ValidPageable only ───────────────────────────────────────────────────

    @Override
    public ResponseEntity<List<Pet>> findPetsWithSizeConstraint(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    @Override
    public ResponseEntity<List<Pet>> findPetsWithPageAndSizeConstraint(Pageable pageable) {
        return ResponseEntity.ok(Collections.emptyList());
    }

    // ── @PageableDefault ─────────────────────────────────────────────────────
    // @PageableDefault(page = 0, size = 25)

    @Override
    public ResponseEntity<List<Pet>> findPetsWithPageSizeDefaultsOnly(Pageable pageable) {
        if (pageable.getPageNumber() != 0) {
            throw new IllegalStateException(
                    "@PageableDefault page: expected 0, got " + pageable.getPageNumber());
        }
        if (pageable.getPageSize() != 25) {
            throw new IllegalStateException(
                    "@PageableDefault size: expected 25, got " + pageable.getPageSize());
        }
        return ResponseEntity.ok(Collections.emptyList());
    }

    // ── @SortDefault ─────────────────────────────────────────────────────────
    // @SortDefault(sort = {"name"}, direction = DESC)

    @Override
    public ResponseEntity<List<Pet>> findPetsWithSortDefaultOnly(Pageable pageable) {
        Sort.Order nameOrder = pageable.getSort().getOrderFor("name");
        if (nameOrder == null || nameOrder.getDirection() != Sort.Direction.DESC) {
            throw new IllegalStateException(
                    "@SortDefault sort: expected name DESC, got " + pageable.getSort());
        }
        return ResponseEntity.ok(Collections.emptyList());
    }

    // @SortDefault(sort = {"id"}, direction = ASC)

    @Override
    public ResponseEntity<List<Pet>> findPetsWithSortDefaultAsc(Pageable pageable) {
        Sort.Order idOrder = pageable.getSort().getOrderFor("id");
        if (idOrder == null || idOrder.getDirection() != Sort.Direction.ASC) {
            throw new IllegalStateException(
                    "@SortDefault sort: expected id ASC, got " + pageable.getSort());
        }
        return ResponseEntity.ok(Collections.emptyList());
    }

    // ── @SortDefault.SortDefaults ─────────────────────────────────────────────
    // @SortDefaults(SortDefault(sort = {"name"}, direction = DESC), SortDefault(sort = {"id"}, direction = ASC))

    @Override
    public ResponseEntity<List<Pet>> findPetsWithMixedSortDefaults(Pageable pageable) {
        Sort.Order nameOrder = pageable.getSort().getOrderFor("name");
        if (nameOrder == null || nameOrder.getDirection() != Sort.Direction.DESC) {
            throw new IllegalStateException(
                    "@SortDefaults sort: expected name DESC, got " + pageable.getSort());
        }
        Sort.Order idOrder = pageable.getSort().getOrderFor("id");
        if (idOrder == null || idOrder.getDirection() != Sort.Direction.ASC) {
            throw new IllegalStateException(
                    "@SortDefaults sort: expected id ASC, got " + pageable.getSort());
        }
        return ResponseEntity.ok(Collections.emptyList());
    }

    // ── @PageableDefault + @SortDefault.SortDefaults combined ─────────────────
    // @PageableDefault(page = 0, size = 10)
    // @SortDefaults(SortDefault(sort = {"name"}, direction = DESC), SortDefault(sort = {"id"}, direction = ASC))

    @Override
    public ResponseEntity<List<Pet>> findPetsWithAllDefaults(Pageable pageable) {
        if (pageable.getPageNumber() != 0) {
            throw new IllegalStateException(
                    "@PageableDefault page: expected 0, got " + pageable.getPageNumber());
        }
        if (pageable.getPageSize() != 10) {
            throw new IllegalStateException(
                    "@PageableDefault size: expected 10, got " + pageable.getPageSize());
        }
        Sort.Order nameOrder = pageable.getSort().getOrderFor("name");
        if (nameOrder == null || nameOrder.getDirection() != Sort.Direction.DESC) {
            throw new IllegalStateException(
                    "@SortDefaults sort: expected name DESC, got " + pageable.getSort());
        }
        Sort.Order idOrder = pageable.getSort().getOrderFor("id");
        if (idOrder == null || idOrder.getDirection() != Sort.Direction.ASC) {
            throw new IllegalStateException(
                    "@SortDefaults sort: expected id ASC, got " + pageable.getSort());
        }
        return ResponseEntity.ok(Collections.emptyList());
    }
}
