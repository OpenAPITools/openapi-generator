<?php
/**
 * HealthCheckResult
 */
namespace app\Models;

/**
 * HealthCheckResult
 * @description Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.
 */
class HealthCheckResult {

    /** @var string|null $nullableMessage */
    public $nullableMessage = null;

}
