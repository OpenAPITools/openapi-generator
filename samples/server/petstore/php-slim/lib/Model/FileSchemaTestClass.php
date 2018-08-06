<?php
/**
 * FileSchemaTestClass
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * FileSchemaTestClass
 */
class FileSchemaTestClass
{

    /** @var \OpenAPIServer\Model\File $file (optional) */
    private $file;

    /** @var \OpenAPIServer\Model\File[] $files (optional) */
    private $files;

    /**
     * FileSchemaTestClass constructor
     *
     * @param \OpenAPIServer\Model\File|null $file (optional)
     * @param \OpenAPIServer\Model\File[]|null $files (optional)
     */
    public function __construct(
        $file = null,
        $files = null
    ) {
        $this->file = $file;
        $this->files = $files;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $fileSchemaTestClass = FileSchemaTestClass::createFromObject([ 'file' => 'foobar' ]);
     *
     * @return FileSchemaTestClass
     */
    public static function createFromObject(array $data = null)
    {
        $file = (isset($data['file'])) ? $data['file'] : null;
        $files = (isset($data['files'])) ? $data['files'] : null;
        return new FileSchemaTestClass(
            $file,
            $files
        );
    }
}
