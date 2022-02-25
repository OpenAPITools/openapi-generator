<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

/**
 * Parameters for uploadFile
 */
class UploadFileParameterData
{
    /**
     * ID of pet to update
     */
    #[DTA\Data(subset: "path", field: "petId")]
    #[DTA\Strategy("QueryStringScalar", ["type" => "int"], "path")]
    #[DTA\Validator("QueryStringScalar", ["type" => "int"], subset: "path")]
    public int|null $pet_id = null;

}
