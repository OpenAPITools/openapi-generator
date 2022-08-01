<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 * Parameters for getUserByName
 */
class GetUserByNameParameterData
{
    /**
     * The name that needs to be fetched. Use user1 for testing.
     * @DTA\Data(subset="path", field="username")
     * @DTA\Strategy(subset="path", name="QueryStringScalar", options={"type":"string"})
     * @DTA\Validator(subset="path", name="QueryStringScalar", options={"type":"string"})
     * @var string|null
     */
    public $username;

}
