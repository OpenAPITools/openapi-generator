<?php
/**
 * ReadOnlyFirst
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ReadOnlyFirst
 */
class ReadOnlyFirst
{

    /** @var string $bar (optional) */
    private $bar;

    /** @var string $baz (optional) */
    private $baz;

    /**
     * ReadOnlyFirst constructor
     *
     * @param string|null $bar (optional)
     * @param string|null $baz (optional)
     */
    public function __construct(
        $bar = null,
        $baz = null
    ) {
        $this->bar = $bar;
        $this->baz = $baz;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $readOnlyFirst = ReadOnlyFirst::createFromObject([ 'bar' => 'foobar' ]);
     *
     * @return ReadOnlyFirst
     */
    public static function createFromObject(array $data = null)
    {
        $bar = (isset($data['bar'])) ? $data['bar'] : null;
        $baz = (isset($data['baz'])) ? $data['baz'] : null;
        return new ReadOnlyFirst(
            $bar,
            $baz
        );
    }
}
