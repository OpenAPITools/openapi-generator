<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for getPetById
 */
class GetPetByIdParameterData
{
    /**
     * ID of pet to return
     */
    #[DTA\Data(subset: "path", field: "petId")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "int"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "int"], subset: "path")]
    public int|null $pet_id = null;

}
