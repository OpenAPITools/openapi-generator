<?php
/**
 * NullableClass
 */
namespace app\Models;

/**
 * NullableClass
 */
class NullableClass {

    /** @var int $integerProp */
    private $integerProp;

    /** @var float $numberProp */
    private $numberProp;

    /** @var bool $booleanProp */
    private $booleanProp;

    /** @var string $stringProp */
    private $stringProp;

    /** @var \DateTime $dateProp */
    private $dateProp;

    /** @var \DateTime $datetimeProp */
    private $datetimeProp;

    /** @var object[] $arrayNullableProp */
    private $arrayNullableProp;

    /** @var object[] $arrayAndItemsNullableProp */
    private $arrayAndItemsNullableProp;

    /** @var object[] $arrayItemsNullable */
    private $arrayItemsNullable;

    /** @var map[string,object] $objectNullableProp */
    private $objectNullableProp;

    /** @var map[string,object] $objectAndItemsNullableProp */
    private $objectAndItemsNullableProp;

    /** @var map[string,object] $objectItemsNullable */
    private $objectItemsNullable;

}
