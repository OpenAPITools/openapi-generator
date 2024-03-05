<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for getOrderById
 */
class GetOrderByIdParameterData
{
    /**
     * ID of pet that needs to be fetched
     */
    #[DTA\Data(subset: "path", field: "orderId")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "int"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "int"], subset: "path")]
    #[DTA\Validator("Range", ["min" => 1, "max" => 5], subset: "path")]
    public int|null $order_id = null;

}
