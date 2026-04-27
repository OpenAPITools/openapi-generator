package org.openapitools.configuration

import jakarta.validation.Constraint
import jakarta.validation.ConstraintValidator
import jakarta.validation.ConstraintValidatorContext
import jakarta.validation.Payload
import org.springframework.data.domain.Pageable

/**
 * Validates that sort properties in the annotated [Pageable] parameter match the allowed values.
 *
 * Apply directly on a `pageable: Pageable` parameter. The validator checks that each sort
 * property and direction combination in the [Pageable] matches one of the strings specified
 * in [allowedValues].
 *
 * Two formats are accepted in [allowedValues]:
 * - `"property,direction"` — permits only the specific direction (e.g. `"id,asc"`, `"name,desc"`).
 *   Direction matching is case-insensitive: `"id,ASC"` and `"id,asc"` are treated identically.
 * - `"property"` — permits any direction for that property (e.g. `"id"` matches `sort=id,asc`
 *   and `sort=id,desc`). Note: because Spring always normalises a bare `sort=id` to ascending
 *   before the validator runs, bare property names in [allowedValues] effectively allow all
 *   directions — the original omission of a direction cannot be detected.
 *
 * Both formats may be mixed freely. For example `["id", "name,desc"]` allows `id` in any
 * direction but restricts `name` to descending only.
 *
 * @property allowedValues The allowed sort strings (e.g. `["id,asc", "id,desc"]`)
 * @property groups Validation groups (optional)
 * @property payload Additional payload (optional)
 * @property message Validation error message (default: "Invalid sort column")
 */
@MustBeDocumented
@Retention(AnnotationRetention.RUNTIME)
@Constraint(validatedBy = [SortValidator::class])
@Target(AnnotationTarget.VALUE_PARAMETER)
annotation class ValidSort(
    val allowedValues: Array<String>,
    val groups: Array<kotlin.reflect.KClass<*>> = [],
    val payload: Array<kotlin.reflect.KClass<out Payload>> = [],
    val message: String = "Invalid sort column"
)

class SortValidator : ConstraintValidator<ValidSort, Pageable> {

    private lateinit var allowedValues: Set<String>

    override fun initialize(constraintAnnotation: ValidSort) {
        allowedValues = constraintAnnotation.allowedValues.map { entry ->
            DIRECTION_ASC_SUFFIX.replace(entry, ",asc")
                .let { DIRECTION_DESC_SUFFIX.replace(it, ",desc") }
        }.toSet()
    }

    private companion object {
        val DIRECTION_ASC_SUFFIX = Regex(",ASC$", RegexOption.IGNORE_CASE)
        val DIRECTION_DESC_SUFFIX = Regex(",DESC$", RegexOption.IGNORE_CASE)
    }

    override fun isValid(pageable: Pageable?, context: ConstraintValidatorContext): Boolean {
        if (pageable == null || pageable.sort.isUnsorted) return true

        val invalid = pageable.sort
            .foldIndexed(emptyMap<Int, String>()) { index, acc, order ->
                val sortValue = "${order.property},${order.direction.name.lowercase()}"
                // Accept "property,direction" (exact match) OR "property" alone (any direction allowed)
                if (sortValue !in allowedValues && order.property !in allowedValues) acc + (index to order.property)
                else acc
            }
            .toSortedMap()

        if (invalid.isNotEmpty()) {
            context.disableDefaultConstraintViolation()
            invalid.forEach { (index, property) ->
                context.buildConstraintViolationWithTemplate(
                    "${context.defaultConstraintMessageTemplate} [$property]"
                )
                    .addPropertyNode("sort")
                    .addPropertyNode("property")
                    .inIterable()
                    .atIndex(index)
                    .addConstraintViolation()
            }
        }

        return invalid.isEmpty()
    }
}
