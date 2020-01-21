<?php
declare(strict_types=1);

namespace App\DTO;

use Articus\DataTransfer\Annotation as DTA;

/**
 */
class FileSchemaTestClass
{
    /**
     * @DTA\Data(field="file", nullable=true)
     * @DTA\Strategy(name="Object", options={"type":\App\DTO\File::class})
     * @DTA\Validator(name="Dictionary", options={"type":\App\DTO\File::class})
     * @var \App\DTO\File
     */
    public $file;
    /**
     * @DTA\Data(field="files", nullable=true)
     * TODO check validator and strategy are correct and can handle container item type
     * @DTA\Strategy(name="ObjectArray", options={"type":\App\DTO\File::class})
     * @DTA\Validator(name="Collection", options={"validators":{
     *     {"name":"Dictionary", "options":{"type":\App\DTO\File::class}}
     * }})
     * @var \App\DTO\File[]
     */
    public $files;
}
