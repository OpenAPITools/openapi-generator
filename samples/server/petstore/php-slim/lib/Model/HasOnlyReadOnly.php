<?php
/**
 * HasOnlyReadOnly
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * HasOnlyReadOnly
 */
class HasOnlyReadOnly
{

    /** @var string $bar (optional) */
    private $bar;

    /** @var string $foo (optional) */
    private $foo;

    /**
     * HasOnlyReadOnly constructor
     *
     * @param string|null $bar (optional)
     * @param string|null $foo (optional)
     */
    public function __construct(
        $bar = null,
        $foo = null
    ) {
        $this->bar = $bar;
        $this->foo = $foo;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $hasOnlyReadOnly = HasOnlyReadOnly::createFromObject([ 'bar' => 'foobar' ]);
     *
     * @return HasOnlyReadOnly
     */
    public static function createFromObject(array $data = null)
    {
        $bar = (isset($data['bar'])) ? $data['bar'] : null;
        $foo = (isset($data['foo'])) ? $data['foo'] : null;
        return new HasOnlyReadOnly(
            $bar,
            $foo
        );
    }
}
