<?php
namespace App\Validator;

use App\Strategy\QueryParameter;

class QueryParameterType extends Type
{
    const RE_INT = '0|-?[1-9]\d*';
    const RE_BOOL = 'true|false';
    const RE_FLOAT = '0(\.\d+)?|-?[1-9]\d*(\.\d+)?|-0\.\d+';

    protected function checkType($value)
    {
        switch ($this->type) {
            case QueryParameter::TYPE_INT:
                return is_string($value) && preg_match('/^(' . self::RE_INT . ')$/', $value);
            case QueryParameter::TYPE_BOOL:
                return is_string($value) && preg_match('/^(' . self::RE_BOOL . ')$/', $value);
            case QueryParameter::TYPE_FLOAT:
                return is_string($value) && preg_match('/^(' . self::RE_FLOAT . ')$/', $value);
            case QueryParameter::TYPE_STRING:
                return is_string($value);
            default:
                throw new \InvalidArgumentException(sprintf('Can not check for type %s.', $this->type));
        }
    }
}
