<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for deleteOrder
 */
class DeleteOrderParameterData
{
    /**
     * ID of the order that needs to be deleted
     */
    #[DTA\Data(subset: "path", field: "orderId")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"], subset: "path")]
    public string|null $order_id = null;

}
