package org.openapitools.configuration;

import jakarta.validation.Constraint;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import jakarta.validation.Payload;
import org.springframework.data.domain.Pageable;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Validates that the page number and page size in the annotated {@link Pageable} parameter do not
 * exceed their configured maximums.
 *
 * <p>Apply directly on a {@code Pageable} parameter. Each attribute is independently optional:
 * <ul>
 *   <li>{@link #maxSize()} — when set (&gt;= 0), validates {@code pageable.getPageSize() <= maxSize}
 *   <li>{@link #maxPage()} — when set (&gt;= 0), validates {@code pageable.getPageNumber() <= maxPage}
 * </ul>
 *
 * <p>Use {@link #NO_LIMIT} (= {@code -1}, the default) to leave an attribute unconstrained.
 *
 * <p>Constraining {@link #maxPage()} is useful to prevent deep-pagination attacks, where a large
 * page offset (e.g. {@code ?page=100000&size=20}) causes an expensive {@code OFFSET} query on the
 * database.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = {ValidPageable.PageableConstraintValidator.class})
@Target({ElementType.PARAMETER})
public @interface ValidPageable {

    /** Sentinel value meaning no limit is applied. */
    int NO_LIMIT = -1;

    /** Maximum allowed page size, or {@link #NO_LIMIT} if unconstrained. */
    int maxSize() default NO_LIMIT;

    /** Maximum allowed page number (0-based), or {@link #NO_LIMIT} if unconstrained. */
    int maxPage() default NO_LIMIT;

    Class<?>[] groups() default {};

    Class<? extends Payload>[] payload() default {};

    String message() default "Invalid page request";

    class PageableConstraintValidator implements ConstraintValidator<ValidPageable, Pageable> {

        private int maxSize = NO_LIMIT;
        private int maxPage = NO_LIMIT;

        @Override
        public void initialize(ValidPageable constraintAnnotation) {
            maxSize = constraintAnnotation.maxSize();
            maxPage = constraintAnnotation.maxPage();
        }

        @Override
        public boolean isValid(Pageable pageable, ConstraintValidatorContext context) {
            if (pageable == null) {
                return true;
            }

            if (!pageable.isPaged()) {
                return true;
            }

            boolean valid = true;
            context.disableDefaultConstraintViolation();

            if (maxSize >= 0 && pageable.getPageSize() > maxSize) {
                context.buildConstraintViolationWithTemplate(
                                context.getDefaultConstraintMessageTemplate()
                                        + ": page size " + pageable.getPageSize()
                                        + " exceeds maximum " + maxSize)
                        .addPropertyNode("size")
                        .addConstraintViolation();
                valid = false;
            }

            if (maxPage >= 0 && pageable.getPageNumber() > maxPage) {
                context.buildConstraintViolationWithTemplate(
                                context.getDefaultConstraintMessageTemplate()
                                        + ": page number " + pageable.getPageNumber()
                                        + " exceeds maximum " + maxPage)
                        .addPropertyNode("page")
                        .addConstraintViolation();
                valid = false;
            }

            return valid;
        }
    }
}
