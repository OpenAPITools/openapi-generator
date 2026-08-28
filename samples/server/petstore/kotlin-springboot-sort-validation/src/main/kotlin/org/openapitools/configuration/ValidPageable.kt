package org.openapitools.configuration

import jakarta.validation.Constraint
import jakarta.validation.ConstraintValidator
import jakarta.validation.ConstraintValidatorContext
import jakarta.validation.Payload
import org.springframework.data.domain.Pageable

/**
 * Validates that the page number and page size in the annotated [Pageable] parameter are within
 * their configured bounds.
 *
 * Apply directly on a `pageable: Pageable` parameter. Each attribute is independently optional:
 * - [maxSize] — when set (>= 0), validates `pageable.pageSize <= maxSize`
 * - [maxPage] — when set (>= 0), validates `pageable.pageNumber <= maxPage`
 * - [minSize] — when set (>= 0), validates `pageable.pageSize >= minSize`
 * - [minPage] — when set (>= 0), validates `pageable.pageNumber >= minPage`
 *
 * Use [NO_LIMIT] (= -1, the default) to leave an attribute unconstrained.
 *
 * Constraining [maxPage] is useful to prevent deep-pagination attacks, where a large page
 * offset (e.g. `?page=100000&size=20`) causes an expensive `OFFSET` query on the database.
 *
 * @property maxSize Maximum allowed page size, or [NO_LIMIT] if unconstrained
 * @property maxPage Maximum allowed page number (0-based), or [NO_LIMIT] if unconstrained
 * @property minSize Minimum allowed page size, or [NO_LIMIT] if unconstrained
 * @property minPage Minimum allowed page number (0-based), or [NO_LIMIT] if unconstrained
 * @property groups Validation groups (optional)
 * @property payload Additional payload (optional)
 * @property message Validation error message (default: "Invalid page request")
 */
@MustBeDocumented
@Retention(AnnotationRetention.RUNTIME)
@Constraint(validatedBy = [PageableConstraintValidator::class])
@Target(AnnotationTarget.VALUE_PARAMETER)
annotation class ValidPageable(
    val maxSize: Int = ValidPageable.NO_LIMIT,
    val maxPage: Int = ValidPageable.NO_LIMIT,
    val minSize: Int = ValidPageable.NO_LIMIT,
    val minPage: Int = ValidPageable.NO_LIMIT,
    val groups: Array<kotlin.reflect.KClass<*>> = [],
    val payload: Array<kotlin.reflect.KClass<out Payload>> = [],
    val message: String = "Invalid page request"
) {
    companion object {
        const val NO_LIMIT = -1
    }
}

class PageableConstraintValidator : ConstraintValidator<ValidPageable, Pageable> {

    private var maxSize = ValidPageable.NO_LIMIT
    private var maxPage = ValidPageable.NO_LIMIT
    private var minSize = ValidPageable.NO_LIMIT
    private var minPage = ValidPageable.NO_LIMIT

    override fun initialize(constraintAnnotation: ValidPageable) {
        maxSize = constraintAnnotation.maxSize
        maxPage = constraintAnnotation.maxPage
        minSize = constraintAnnotation.minSize
        minPage = constraintAnnotation.minPage
    }

    override fun isValid(pageable: Pageable?, context: ConstraintValidatorContext): Boolean {
        if (pageable == null) return true
        if (!pageable.isPaged) return true

        var valid = true
        context.disableDefaultConstraintViolation()

        if (maxSize >= 0 && pageable.pageSize > maxSize) {
            context.buildConstraintViolationWithTemplate(
                "${context.defaultConstraintMessageTemplate}: page size ${pageable.pageSize} exceeds maximum $maxSize"
            )
                .addPropertyNode("size")
                .addConstraintViolation()
            valid = false
        }

        if (maxPage >= 0 && pageable.pageNumber > maxPage) {
            context.buildConstraintViolationWithTemplate(
                "${context.defaultConstraintMessageTemplate}: page number ${pageable.pageNumber} exceeds maximum $maxPage"
            )
                .addPropertyNode("page")
                .addConstraintViolation()
            valid = false
        }

        if (minSize >= 0 && pageable.pageSize < minSize) {
            context.buildConstraintViolationWithTemplate(
                "${context.defaultConstraintMessageTemplate}: page size ${pageable.pageSize} is below minimum $minSize"
            )
                .addPropertyNode("size")
                .addConstraintViolation()
            valid = false
        }

        if (minPage >= 0 && pageable.pageNumber < minPage) {
            context.buildConstraintViolationWithTemplate(
                "${context.defaultConstraintMessageTemplate}: page number ${pageable.pageNumber} is below minimum $minPage"
            )
                .addPropertyNode("page")
                .addConstraintViolation()
            valid = false
        }

        return valid
    }
}
