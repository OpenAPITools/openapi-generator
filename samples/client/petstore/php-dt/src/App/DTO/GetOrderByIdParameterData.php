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
     * @DTA\Validator(subset="path", name="Range", options={"min":1, "max":5})
     */
    public ?int $order_id = null;

}
