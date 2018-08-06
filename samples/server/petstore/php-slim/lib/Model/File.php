<?php
/**
 * File
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * File
 */
class File
{

    /** @var string $sourceURI (optional) Test capitalization */
    private $sourceURI;

    /**
     * File constructor
     *
     * @param string|null $sourceURI (optional) Test capitalization
     */
    public function __construct(
        $sourceURI = null
    ) {
        $this->sourceURI = $sourceURI;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $file = File::createFromObject([ 'sourceURI' => 'foobar' ]);
     *
     * @return File
     */
    public static function createFromObject(array $data = null)
    {
        $sourceURI = (isset($data['sourceURI'])) ? $data['sourceURI'] : null;
        return new File(
            $sourceURI
        );
    }
}
