<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for deleteOrder
 */
class DeleteOrderParameterData
{
    /**
     * ID of the order that needs to be deleted
     * @DTA\Data(subset="path", field="orderId")
     * @DTA\Strategy(subset="path", name="QueryStringScalar", options={"type":"string"})
     * @DTA\Validator(subset="path", name="QueryStringScalar", options={"type":"string"})
     * @var string|null
     */
    public $order_id;

}
