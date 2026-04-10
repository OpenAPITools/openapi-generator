package org.openapitools.configuration

import jakarta.validation.Constraint
import jakarta.validation.ConstraintValidator
import jakarta.validation.ConstraintValidatorContext
import jakarta.validation.Payload
import jakarta.validation.constraintvalidation.SupportedValidationTarget
import jakarta.validation.constraintvalidation.ValidationTarget
import org.springframework.data.domain.Pageable

/**
 * Validates that sort properties in a [Pageable] parameter match the allowed values.
 *
 * This annotation can only be applied to methods that have a [Pageable] parameter.
 * The validator checks that each sort property and direction combination in the [Pageable]
 * matches one of the strings specified in [allowedValues].
 *
 * Expected value format: `"property,direction"` (e.g. `"id,asc"`, `"name,desc"`).
 *
 * @property allowedValues The allowed sort strings (e.g. `["id,asc", "id,desc"]`)
 * @property groups Validation groups (optional)
 * @property payload Additional payload (optional)
 * @property message Validation error message (default: "Invalid sort column")
 */
@MustBeDocumented
@Retention(AnnotationRetention.RUNTIME)
@Constraint(validatedBy = [SortValidator::class])
@Target(AnnotationTarget.FUNCTION)
annotation class ValidSort(
    val allowedValues: Array<String>,
    val groups: Array<kotlin.reflect.KClass<*>> = [],
    val payload: Array<kotlin.reflect.KClass<out Payload>> = [],
    val message: String = "Invalid sort column"
)

@SupportedValidationTarget(ValidationTarget.PARAMETERS)
class SortValidator : ConstraintValidator<ValidSort, Array<Any?>> {

    private lateinit var allowedValues: Set<String>

    override fun initialize(constraintAnnotation: ValidSort) {
        allowedValues = constraintAnnotation.allowedValues.toSet()
    }

    override fun isValid(parameters: Array<Any?>?, context: ConstraintValidatorContext): Boolean {
        val pageable = parameters?.filterIsInstance<Pageable>()?.firstOrNull()
            ?: throw IllegalStateException(
                "@ValidSort can only be used on methods with a Pageable parameter. " +
                    "Ensure the annotated method has a parameter of type org.springframework.data.domain.Pageable."
            )

        val invalid = pageable.sort
            .foldIndexed(emptyMap<Int, String>()) { index, acc, order ->
                val sortValue = "${order.property},${order.direction.name.lowercase()}"
                if (sortValue !in allowedValues) acc + (index to order.property)
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
