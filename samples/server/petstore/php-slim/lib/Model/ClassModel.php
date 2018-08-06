<?php
/**
 * ClassModel
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ClassModel
 */
class ClassModel
{

    /** @var string $class (optional) */
    private $class;

    /**
     * ClassModel constructor
     *
     * @param string|null $class (optional)
     */
    public function __construct(
        $class = null
    ) {
        $this->class = $class;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $classModel = ClassModel::createFromObject([ '_class' => 'foobar' ]);
     *
     * @return ClassModel
     */
    public static function createFromObject(array $data = null)
    {
        $class = (isset($data['_class'])) ? $data['_class'] : null;
        return new ClassModel(
            $class
        );
    }
}
