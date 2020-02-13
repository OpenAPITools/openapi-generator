<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class InlineResponseDefault
{
    /**
     * @DTA\Data(field="string", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\Foo::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\Foo::class})
     * @var \App\DTO\Foo
     */
    public $string;
}
