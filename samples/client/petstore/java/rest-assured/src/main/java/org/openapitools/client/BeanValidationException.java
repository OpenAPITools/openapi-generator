package org.openapitools.client;

import java.util.Set;

import javax.validation.ConstraintViolation;
import javax.validation.ValidationException;

public class BeanValidationException extends ValidationException {
    /**
     *
     */
    private static final long serialVersionUID = -5294733947409491364L;
    Set<ConstraintViolation<Object>> violations;

    public BeanValidationException(Set<ConstraintViolation<Object>> violations) {
        this.violations = violations;
    }

    public Set<ConstraintViolation<Object>> getViolations() {
        return violations;
    }

    public void setViolations(Set<ConstraintViolation<Object>> violations) {
        this.violations = violations;
    }

}
