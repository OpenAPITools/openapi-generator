<?php
/**
 * NullableClass
 */
namespace app\Models;

/**
 * NullableClass
 */
class NullableClass {

    /** @var int|null $integerProp */
    public $integerProp = null;

    /** @var float|null $numberProp */
    public $numberProp = null;

    /** @var bool|null $booleanProp */
    public $booleanProp = null;

    /** @var string|null $stringProp */
    public $stringProp = null;

    /** @var \DateTime|null $dateProp */
    public $dateProp = null;

    /** @var \DateTime|null $datetimeProp */
    public $datetimeProp = null;

    /** @var object[]|null $arrayNullableProp */
    public $arrayNullableProp = null;

    /** @var object[]|null $arrayAndItemsNullableProp */
    public $arrayAndItemsNullableProp = null;

    /** @var object[] $arrayItemsNullable */
    public $arrayItemsNullable = [];

    /** @var array<string,object>|null $objectNullableProp */
    public $objectNullableProp = null;

    /** @var array<string,object>|null $objectAndItemsNullableProp */
    public $objectAndItemsNullableProp = null;

    /** @var array<string,object> $objectItemsNullable */
    public $objectItemsNullable;

}
