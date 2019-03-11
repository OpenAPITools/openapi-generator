<?php
declare(strict_types=1);

namespace App\Strategy;

use Articus\DataTransfer\Strategy\StrategyInterface;

class DateTime implements StrategyInterface
{
    const DATE_TIME_FORMAT = \DateTime::RFC3339;

    /**
     * @inheritdoc
     */
    public function extract($objectValue, $object = null)
    {
        $result = null;
        if ($objectValue instanceof \DateTime) {
            $result = $objectValue->format(static::DATE_TIME_FORMAT);
        }
        return $result;
    }

    /**
     * @inheritdoc
     */
    public function hydrate($arrayValue, $objectValue, array $array = null)
    {
        $result = null;
        if (!empty($arrayValue)) {
            $date = $this->parseDateString($arrayValue);
            if ($date instanceof \DateTime) {
                $result = $date;
            }
        }
        return $result;
    }

    /**
     * @param $arrayValue
     * @return bool|\DateTime
     */
    protected function parseDateString($arrayValue)
    {
        return \DateTime::createFromFormat(static::DATE_TIME_FORMAT, $arrayValue, new \DateTimeZone('UTC'));
    }
}
