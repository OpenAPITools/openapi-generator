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

    /** @var int $pet_id */
    private $pet_id;

    /** @var int $quantity */
    private $quantity;

    /** @var \DateTime $ship_date */
    private $ship_date;

    /** @var string $status Order Status*/
    private $status;

    /** @var bool $complete */
    private $complete;

}
