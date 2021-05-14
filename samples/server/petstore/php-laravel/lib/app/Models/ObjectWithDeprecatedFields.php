<?php
/**
 * ObjectWithDeprecatedFields
 */
namespace app\Models;

/**
 * ObjectWithDeprecatedFields
 */
class ObjectWithDeprecatedFields {

    /** @var string $uuid */
    private $uuid;

    /** @var float $id */
    private $id;

    /** @var \app\Models\DeprecatedObject $deprecatedRef */
    private $deprecatedRef;

    /** @var string[] $bars */
    private $bars;

}
