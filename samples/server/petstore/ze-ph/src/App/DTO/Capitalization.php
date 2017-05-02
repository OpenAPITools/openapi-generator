<?php

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class Capitalization 
{
    /**
     * @DTA\Data(field="smallCamel", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $small_camel;
    /**
     * @DTA\Data(field="CapitalCamel", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $capital_camel;
    /**
     * @DTA\Data(field="small_Snake", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $small_snake;
    /**
     * @DTA\Data(field="Capital_Snake", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $capital_snake;
    /**
     * @DTA\Data(field="SCA_ETH_Flow_Points", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $sca_eth_flow_points;
    /**
     * Name of the pet 
     * @DTA\Data(field="ATT_NAME", nullable=true)
     * @DTA\Validator(name="Type", options={"type":"string"})
     * @var string
     */
    public $att_name;
}

