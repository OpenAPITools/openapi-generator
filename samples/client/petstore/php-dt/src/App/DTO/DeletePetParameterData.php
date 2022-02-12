<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for deletePet
 */
class DeletePetParameterData
{
    /**
     * Pet id to delete
     * @DTA\Data(subset="path", field="petId")
     * @DTA\Strategy(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @DTA\Validator(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @var int
     */
    public $pet_id = 0;

    /**
     * @DTA\Data(subset="header", field="api_key")
     * @DTA\Strategy(subset="header", name="QueryStringScalar", options={"type":"string"})
     * @DTA\Validator(subset="header", name="QueryStringScalar", options={"type":"string"})
     * @var string
     */
    public $api_key = "";

}
