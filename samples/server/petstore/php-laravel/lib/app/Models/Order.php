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
    private $id = 0;

    /** @var int $petId */
    private $petId = 0;

    /** @var int $quantity */
    private $quantity = 0;

    /** @var \DateTime $shipDate */
    private $shipDate;

    /** @var string $status Order Status*/
    private $status = "";

    /** @var bool $complete */
    private $complete = false;

}
