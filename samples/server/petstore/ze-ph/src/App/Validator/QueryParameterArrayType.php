<?php
namespace App\Validator;

use App\Strategy\QueryParameterArray;

class QueryParameterArrayType extends QueryParameterType
{
    /**
     * @var string
     */
    protected $format;

    /**
     * @return string
     */
    public function getFormat()
    {
        return $this->format;
    }

    /**
     * @param string $format
     * @return self
     */
    public function setFormat($format)
    {
        $this->format = $format;
        return $this;
    }

    protected function checkType($value)
    {
        $result = true;
        if (!array_key_exists($this->format, QueryParameterArray::DELIMITER_MAP)) {
            throw new \InvalidArgumentException(sprintf('Can not check for format %s.', $this->format));
        }
        $delimiter = QueryParameterArray::DELIMITER_MAP[$this->format];
        if ($delimiter === null) {
            if (is_array($value)) {
                foreach ($value as $item) {
                    $result = $result && parent::checkType($item);
                }
            } else {
                $result = false;
            }
        } else {
            switch ($this->type) {
                case QueryParameterArray::TYPE_INT:
                    $result = is_string($value) && preg_match(self::prepareRepeatingTypeRegExp(self::RE_INT, $delimiter), $value);
                    break;
                case QueryParameterArray::TYPE_BOOL:
                    $result = is_string($value) && preg_match(self::prepareRepeatingTypeRegExp(self::RE_BOOL, $delimiter), $value);
                    break;
                case QueryParameterArray::TYPE_FLOAT:
                    $result = is_string($value) && preg_match(self::prepareRepeatingTypeRegExp(self::RE_FLOAT, $delimiter), $value);
                    break;
                case QueryParameterArray::TYPE_STRING:
                    $result = is_string($value);
                    break;
                default:
                    throw new \InvalidArgumentException(sprintf('Can not check for type %s.', $this->type));
            }
        }
        return $result;
    }

    protected static function prepareRepeatingTypeRegExp($typeRegExp, $delimiter)
    {
        $escapedDelimiter = preg_quote($delimiter, '/');
        return '/^(' . $typeRegExp . ')(' . $escapedDelimiter . '('. $typeRegExp . '))*$/';
    }
}
