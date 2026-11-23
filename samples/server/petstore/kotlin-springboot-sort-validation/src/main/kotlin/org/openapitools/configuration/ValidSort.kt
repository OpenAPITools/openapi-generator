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
 * The number of sort orders can additionally be bounded with [minItems] and [maxItems]. An unsorted
 * [Pageable] (no sort given) is always accepted, even when `minItems > 0`; the bounds only apply
 * once at least one sort order is present.
 *
 * @property allowedValues The allowed sort strings (e.g. `["id,asc", "id,desc"]`)
 * @property groups Validation groups (optional)
 * @property payload Additional payload (optional)
 * @property message Validation error message (default: "Invalid sort column")
 * @property minItems The minimum number of sort orders (default: unbounded)
 * @property maxItems The maximum number of sort orders (default: unbounded)
 */
@MustBeDocumented
@Retention(AnnotationRetention.RUNTIME)
@Constraint(validatedBy = [SortValidator::class])
@Target(AnnotationTarget.VALUE_PARAMETER)
annotation class ValidSort(
    val allowedValues: Array<String>,
    val groups: Array<kotlin.reflect.KClass<*>> = [],
    val payload: Array<kotlin.reflect.KClass<out Payload>> = [],
    val message: String = "Invalid sort column",
    val minItems: Int = 0,
    val maxItems: Int = Int.MAX_VALUE
)

class SortValidator : ConstraintValidator<ValidSort, Pageable> {

    private lateinit var allowedValues: Set<String>
    private var minItems: Int = 0
    private var maxItems: Int = Int.MAX_VALUE

    override fun initialize(constraintAnnotation: ValidSort) {
        allowedValues = constraintAnnotation.allowedValues.map { entry ->
            DIRECTION_ASC_SUFFIX.replace(entry, ",asc")
                .let { DIRECTION_DESC_SUFFIX.replace(it, ",desc") }
        }.toSet()
        minItems = constraintAnnotation.minItems
        maxItems = constraintAnnotation.maxItems
    }

    private companion object {
        val DIRECTION_ASC_SUFFIX = Regex(",ASC$", RegexOption.IGNORE_CASE)
        val DIRECTION_DESC_SUFFIX = Regex(",DESC$", RegexOption.IGNORE_CASE)
        val MESSAGE_TEMPLATE_SPECIAL_CHARS = Regex("""[\\{}$]""")

        /** Escapes user input so the message interpolator treats it as literal text. */
        fun escapeMessageTemplate(value: String): String =
            MESSAGE_TEMPLATE_SPECIAL_CHARS.replace(value) { "\\" + it.value }
    }

    override fun isValid(pageable: Pageable?, context: ConstraintValidatorContext): Boolean {
        if (pageable == null || pageable.sort.isUnsorted) return true

        val count = pageable.sort.count()
        val countValid = count in minItems..maxItems

        val invalid = pageable.sort
            .foldIndexed(emptyMap<Int, String>()) { index, acc, order ->
                val sortValue = "${order.property},${order.direction.name.lowercase()}"
                // Accept "property,direction" (exact match) OR "property" alone (any direction allowed)
                if (sortValue !in allowedValues && order.property !in allowedValues) acc + (index to order.property)
                else acc
            }
            .toSortedMap()

        if (!countValid || invalid.isNotEmpty()) {
            context.disableDefaultConstraintViolation()
        }
        if (!countValid) {
            val range = when {
                maxItems == Int.MAX_VALUE -> "at least $minItems"
                minItems == 0 -> "at most $maxItems"
                else -> "$minItems..$maxItems"
            }
            context.buildConstraintViolationWithTemplate(
                "${context.defaultConstraintMessageTemplate} [expected $range sort order(s), got $count]"
            )
                .addPropertyNode("sort")
                .addConstraintViolation()
        }
        invalid.forEach { (index, property) ->
            context.buildConstraintViolationWithTemplate(
                "${context.defaultConstraintMessageTemplate} [${escapeMessageTemplate(property)}]"
            )
                .addPropertyNode("sort")
                .addPropertyNode("property")
                .inIterable()
                .atIndex(index)
                .addConstraintViolation()
        }

        return countValid && invalid.isEmpty()
    }
}
