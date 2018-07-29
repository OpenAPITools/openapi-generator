<?php
namespace App\Validator;

use Zend\Validator\AbstractValidator;
use Zend\Validator\Exception;

class Type extends AbstractValidator
{
    const INVALID = 'typeInvalid';

    /**
     * Validation failure message template definitions
     *
     * @var array
     */
    protected $messageTemplates = [
        self::INVALID => 'Invalid type given.',
    ];

    /**
     * @var string
     */
    protected $type;

    /**
     * @return mixed
     */
    public function getType()
    {
        return $this->type;
    }

    /**
     * @param string $type
     * @return self
     */
    public function setType($type)
    {
        $this->type = $type;
        return $this;
    }

    /**
     * @inheritDoc
     */
    public function isValid($value)
    {
        $result = true;
        if (!$this->checkType($value)) {
            $this->error(self::INVALID);
            $result = false;
        }
        return $result;
    }

    protected function checkType($value)
    {
        switch ($this->type) {
            case 'int':
                return is_int($value);
            case 'bool':
                return is_bool($value);
            case 'float':
                return is_float($value) || is_int($value);
            case 'string':
                return is_string($value);
            default:
                throw new \InvalidArgumentException(sprintf('Can not check for type %s.', $this->type));
        }
    }
}
