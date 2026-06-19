<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for deletePet
 */
class DeletePetParameterData
{
    /**
     * Pet id to delete
     */
    #[DTA\Data(subset: "path", field: "petId")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "int"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "int"], subset: "path")]
    public int|null $pet_id = null;

    #[DTA\Data(subset: "header", field: "api_key", nullable: true)]
    #[DTA\Strategy("QueryStringScalar", ["type" => "string"], "header")]
    #[DTA\Validator("QueryStringScalar", ["type" => "string"], subset: "header")]
    public string|null $api_key = null;

}
