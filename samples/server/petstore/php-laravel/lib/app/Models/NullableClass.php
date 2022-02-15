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
    private $integerProp = null;

    /** @var float|null $numberProp */
    private $numberProp = null;

    /** @var bool|null $booleanProp */
    private $booleanProp = null;

    /** @var string|null $stringProp */
    private $stringProp = null;

    /** @var \DateTime|null $dateProp */
    private $dateProp = null;

    /** @var \DateTime|null $datetimeProp */
    private $datetimeProp = null;

    /** @var object[]|null $arrayNullableProp */
    private $arrayNullableProp = null;

    /** @var object[]|null $arrayAndItemsNullableProp */
    private $arrayAndItemsNullableProp = null;

    /** @var object[] $arrayItemsNullable */
    private $arrayItemsNullable = [];

    /** @var array<string,object>|null $objectNullableProp */
    private $objectNullableProp = null;

    /** @var array<string,object>|null $objectAndItemsNullableProp */
    private $objectAndItemsNullableProp = null;

    /** @var array<string,object> $objectItemsNullable */
    private $objectItemsNullable;

}
