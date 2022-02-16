<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for getOrderById
 */
class GetOrderByIdParameterData
{
    /**
     * ID of pet that needs to be fetched
     * @DTA\Data(subset="path", field="orderId")
     * @DTA\Strategy(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @DTA\Validator(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @DTA\Validator(subset="path", name="GreaterThan", options={"min":1, "inclusive":true})
     * @DTA\Validator(subset="path", name="LessThan", options={"max":5, "inclusive":true})
     * @var int|null
     */
    public $order_id;

}
