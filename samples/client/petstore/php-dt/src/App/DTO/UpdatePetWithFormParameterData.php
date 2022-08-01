<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for updatePetWithForm
 */
class UpdatePetWithFormParameterData
{
    /**
     * ID of pet that needs to be updated
     * @DTA\Data(subset="path", field="petId")
     * @DTA\Strategy(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @DTA\Validator(subset="path", name="QueryStringScalar", options={"type":"int"})
     * @var int|null
     */
    public $pet_id;

}
