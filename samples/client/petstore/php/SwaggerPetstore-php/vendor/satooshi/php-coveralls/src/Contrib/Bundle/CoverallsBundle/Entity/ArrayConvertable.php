<?php
namespace Contrib\Bundle\CoverallsBundle\Entity;

/**
 * Array convertable entity.
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
interface ArrayConvertable
{
    /**
     * Convert to an array.
     *
     * @return array
     */
    public function toArray();
}
