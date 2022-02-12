<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * An order for a pets from the pet store
 */
class Order
{
    /**
     * @DTA\Data(field="id")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $id = 0;

    /**
     * @DTA\Data(field="petId")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $pet_id = 0;

    /**
     * @DTA\Data(field="quantity")
     * @DTA\Validator(name="Scalar", options={"type":"int"})
     * @var int
     */
    public $quantity = 0;

    /**
     * @DTA\Data(field="shipDate", nullable=true)
     * @DTA\Strategy(name="DateTime")
     * @DTA\Validator(name="Date", options={"format": \DateTime::RFC3339})
     * @var \DateTime|null
     */
    public $ship_date;

    /**
     * Order Status
     * @DTA\Data(field="status")
     * @DTA\Validator(name="Scalar", options={"type":"string"})
     * @var string
     */
    public $status = "";

    /**
     * @DTA\Data(field="complete")
     * @DTA\Validator(name="Scalar", options={"type":"bool"})
     * @var bool
     */
    public $complete = false;

}
