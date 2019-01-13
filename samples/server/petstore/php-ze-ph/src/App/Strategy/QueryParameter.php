<?php
declare(strict_types=1);

namespace App\Strategy;

use Articus\DataTransfer\Strategy\StrategyInterface;

class QueryParameter implements StrategyInterface
{
    const TYPE_INT = 'int';
    const TYPE_FLOAT = 'float';
    const TYPE_BOOL = 'bool';
    const TYPE_STRING = 'string';

    const TYPE_MAP = [
        self::TYPE_INT => true,
        self::TYPE_FLOAT => true,
        self::TYPE_BOOL => true,
        self::TYPE_STRING => true,
    ];

    /**
     * @var string
     */
    protected $type;

    /**
     * QueryParameterArray constructor.
     */
    public function __construct(array $options)
    {
        if (empty($options['type'])) {
            throw new \InvalidArgumentException('Option "type" is required.');
        } elseif (!isset(self::TYPE_MAP[$options['type']])) {
            throw new \InvalidArgumentException(\sprintf('Unknown type "%s".', $options['type']));
        }
        $this->type = $options['type'];
    }


    /**
     * @inheritdoc
     */
    public function extract($objectValue, $object = null)
    {
        $result = null;
        if ($objectValue !== null) {
            $result = (string)$objectValue;
        }
        return $result;
    }

    /**
     * @inheritdoc
     */
    public function hydrate($arrayValue, $objectValue, array $array = null)
    {
        $result = null;
        if ($arrayValue !== null) {
            switch ($this->type) {
                case self::TYPE_INT:
                    $result = (int)$arrayValue;
                    break;
                case self::TYPE_FLOAT:
                    $result = (float)$arrayValue;
                    break;
                case self::TYPE_BOOL:
                    $result = ($arrayValue === 'true')? true : false;
                    break;
                case self::TYPE_STRING:
                    $result = (string)$arrayValue;
                    break;
            }
        }
        return $result;
    }
}
