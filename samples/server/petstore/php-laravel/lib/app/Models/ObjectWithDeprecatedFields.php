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
    public $uuid = "";

    /** @var float $id */
    /** @deprecated */
    public $id = 0;

    /** @var \app\Models\DeprecatedObject $deprecatedRef */
    /** @deprecated */
    public $deprecatedRef;

    /** @var string[] $bars */
    /** @deprecated */
    public $bars = [];

}
