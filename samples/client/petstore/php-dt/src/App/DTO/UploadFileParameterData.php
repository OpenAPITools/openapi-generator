<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for uploadFile
 */
class UploadFileParameterData
{
    /**
     * ID of pet to update
     * @DTA\Data(subset="path", field="petId")
     * @DTA\Strategy(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @DTA\Validator(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @var int|null
     */
    public $pet_id;

}
