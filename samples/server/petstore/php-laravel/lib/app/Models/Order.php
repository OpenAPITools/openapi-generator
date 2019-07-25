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
    private $id;

    /** @var int $petId */
    private $petId;

    /** @var int $quantity */
    private $quantity;

    /** @var \DateTime $shipDate */
    private $shipDate;

    /** @var string $status Order Status*/
    private $status;

    /** @var bool $complete */
    private $complete;

}
