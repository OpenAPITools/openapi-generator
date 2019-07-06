<?php
/**
 * AdditionalPropertiesClass
 */
namespace app\Models;

/**
 * AdditionalPropertiesClass
 */
class AdditionalPropertiesClass {

    /** @var map[string,string] $mapString */
    private $mapString;

    /** @var map[string,float] $mapNumber */
    private $mapNumber;

    /** @var map[string,int] $mapInteger */
    private $mapInteger;

    /** @var map[string,bool] $mapBoolean */
    private $mapBoolean;

    /** @var map[string,int[]] $mapArrayInteger */
    private $mapArrayInteger;

    /** @var map[string,object[]] $mapArrayAnytype */
    private $mapArrayAnytype;

    /** @var map[string,map[string,string]] $mapMapString */
    private $mapMapString;

    /** @var map[string,map[string,object]] $mapMapAnytype */
    private $mapMapAnytype;

    /** @var object $anytype1 */
    private $anytype1;

    /** @var object $anytype2 */
    private $anytype2;

    /** @var object $anytype3 */
    private $anytype3;

}
