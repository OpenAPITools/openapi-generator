<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\PhpAttribute as DTA;

#[DTA\Strategy("ObjectList", ["type" => \App\DTO\Pet::class])]
#[DTA\Validator("Collection", ["validators" => [
    ["name" => "TypeCompliant", "options" => ["type" => \App\DTO\Pet::class]]
]])]
class Collection22 extends \ArrayObject
{
}
