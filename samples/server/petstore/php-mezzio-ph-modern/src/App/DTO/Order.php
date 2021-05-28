<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * An order for a pets from the pet store
 */
class Order
{
    #[DTA\Data(field: "id", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $id = null;

    #[DTA\Data(field: "petId", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $pet_id = null;

    #[DTA\Data(field: "quantity", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "int"])]
    public int|null $quantity = null;

    #[DTA\Data(field: "shipDate", nullable: true)]
    #[DTA\Strategy("DateTime")]
    #[DTA\Validator("Date", ["format" => \DateTime::RFC3339])]
    public \DateTime|null $ship_date = null;

    /**
     * Order Status
     */
    #[DTA\Data(field: "status", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "string"])]
    public string|null $status = null;

    #[DTA\Data(field: "complete", nullable: true)]
    #[DTA\Validator("Scalar", ["type" => "bool"])]
    public bool|null $complete = null;

}
