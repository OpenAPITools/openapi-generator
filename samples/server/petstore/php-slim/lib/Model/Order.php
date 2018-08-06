<?php
/**
 * Order
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Order
 */
class Order
{

    /** @var int $id (optional) */
    private $id;

    /** @var int $petId (optional) */
    private $petId;

    /** @var int $quantity (optional) */
    private $quantity;

    /** @var \DateTime $shipDate (optional) */
    private $shipDate;

    /** @var string $status (optional) Order Status */
    private $status;

    /** @var bool $complete (optional) Default false */
    private $complete = false;

    /**
     * Order constructor
     *
     * @param int|null $id (optional)
     * @param int|null $petId (optional)
     * @param int|null $quantity (optional)
     * @param \DateTime|null $shipDate (optional)
     * @param string|null $status (optional) Order Status
     * @param bool|null $complete (optional) Default false
     */
    public function __construct(
        $id = null,
        $petId = null,
        $quantity = null,
        $shipDate = null,
        $status = null,
        $complete = false
    ) {
        $this->id = $id;
        $this->petId = $petId;
        $this->quantity = $quantity;
        $this->shipDate = $shipDate;
        $this->status = $status;
        $this->complete = $complete;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $order = Order::createFromObject([ 'id' => 'foobar' ]);
     *
     * @return Order
     */
    public static function createFromObject(array $data = null)
    {
        $id = (isset($data['id'])) ? $data['id'] : null;
        $petId = (isset($data['petId'])) ? $data['petId'] : null;
        $quantity = (isset($data['quantity'])) ? $data['quantity'] : null;
        $shipDate = (isset($data['shipDate'])) ? $data['shipDate'] : null;
        $status = (isset($data['status'])) ? $data['status'] : null;
        $complete = (isset($data['complete'])) ? $data['complete'] : false;
        return new Order(
            $id,
            $petId,
            $quantity,
            $shipDate,
            $status,
            $complete
        );
    }
}
