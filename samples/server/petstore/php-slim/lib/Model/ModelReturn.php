<?php
/**
 * ModelReturn
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ModelReturn
 */
class ModelReturn
{

    /** @var int $return (optional) */
    private $return;

    /**
     * ModelReturn constructor
     *
     * @param int|null $return (optional)
     */
    public function __construct(
        $return = null
    ) {
        $this->return = $return;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $return = ModelReturn::createFromObject([ 'return' => 'foobar' ]);
     *
     * @return ModelReturn
     */
    public static function createFromObject(array $data = null)
    {
        $return = (isset($data['return'])) ? $data['return'] : null;
        return new ModelReturn(
            $return
        );
    }
}
