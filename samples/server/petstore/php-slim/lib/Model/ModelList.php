<?php
/**
 * ModelList
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ModelList
 */
class ModelList
{

    /** @var string $_123list (optional) */
    private $_123list;

    /**
     * ModelList constructor
     *
     * @param string|null $_123list (optional)
     */
    public function __construct(
        $_123list = null
    ) {
        $this->_123list = $_123list;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $list = ModelList::createFromObject([ '123-list' => 'foobar' ]);
     *
     * @return ModelList
     */
    public static function createFromObject(array $data = null)
    {
        $_123list = (isset($data['123-list'])) ? $data['123-list'] : null;
        return new ModelList(
            $_123list
        );
    }
}
