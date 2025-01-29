<?php
/**
 * Order
 */
namespace app\Models;

/**
 * Order
 */
class Order {

    /** @var int $id */
    public $id = 0;

    /** @var int $petId */
    public $petId = 0;

    /** @var int $quantity */
    public $quantity = 0;

    /** @var \DateTime $shipDate */
    public $shipDate;

    /** @var string $status Order Status*/
    public $status = "";

    /** @var bool $complete */
    public $complete = false;

}
