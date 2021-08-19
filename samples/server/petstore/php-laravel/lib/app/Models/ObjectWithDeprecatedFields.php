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
    /** @deprecated */
    private $id;

    /** @var \app\Models\DeprecatedObject $deprecatedRef */
    /** @deprecated */
    private $deprecatedRef;

    /** @var string[] $bars */
    /** @deprecated */
    private $bars;

}
