<?php

namespace Prophecy\Util;

use Prophecy\Prophecy\ProphecyInterface;
use SplObjectStorage;

/*
 * This file is part of the Prophecy.
 * (c) Konstantin Kudryashov <ever.zet@gmail.com>
 *     Marcello Duarte <marcello.duarte@gmail.com>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Exporting utility.
 *
 * This class is derived from the PHPUnit testing framework.
 *
 * @author  Sebastiaan Stok <s.stok@rollerscapes.net
 * @author  Sebastian Bergmann <sebastian@phpunit.de>
 * @license http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License>
 */
class ExportUtil
{
    /**
     * Exports a value into a string.
     *
     * The output of this method is similar to the output of print_r(), but
     * improved in various aspects:
     *
     *  - NULL is rendered as "null" (instead of "")
     *  - true is rendered as "true" (instead of "1")
     *  - FALSE is rendered as "false" (instead of "")
     *  - Strings are always quoted with single quotes
     *  - Carriage returns and newlines are normalized to \n
     *  - Recursion and repeated rendering is treated properly
     *
     * @param  mixed   $value       The value to export
     * @param  integer $indentation The indentation level of the 2nd+ line
     *
     * @return string
     */
    public static function export($value, $indentation = 0)
    {
        return static::recursiveExport($value, $indentation);
    }

    /**
     * Converts an object to an array containing all of its private, protected
     * and public properties.
     *
     * @param  object $object
     *
     * @return array
     */
    public static function toArray($object)
    {
        $array = array();

        foreach ((array) $object as $key => $value) {
            // properties are transformed to keys in the following way:

            // private   $property => "\0Classname\0property"
            // protected $property => "\0*\0property"
            // public    $property => "property"

            if (preg_match('/^\0.+\0(.+)$/', $key, $matches)) {
                $key = $matches[1];
            }

            $array[$key] = $value;
        }

        // Some internal classes like SplObjectStorage don't work with the
        // above (fast) mechanism nor with reflection
        // Format the output similarly to print_r() in this case
        if ($object instanceof SplObjectStorage) {
            foreach ($object as $key => $value) {
                $array[spl_object_hash($value)] = array(
                    'obj' => $value,
                    'inf' => $object->getInfo(),
                );
            }
        }

        return $array;
    }

    /**
     * Recursive implementation of export.
     *
     * @param  mixed   $value            The value to export
     * @param  integer $indentation      The indentation level of the 2nd+ line
     * @param  array   $processedObjects Contains all objects that were already
     *                                   rendered
     *
     * @return string
     */
    protected static function recursiveExport($value, $indentation, &$processedObjects = array())
    {
        if ($value === null) {
            return 'null';
        }

        if ($value === true) {
            return 'true';
        }

        if ($value === false) {
            return 'false';
        }

        if (is_string($value)) {
            // Match for most non printable chars somewhat taking multibyte chars into account
            if (preg_match('/[^\x09-\x0d\x20-\xff]/', $value)) {
                return 'Binary String: 0x' . bin2hex($value);
            }

            return "'" . str_replace(array("\r\n", "\n\r", "\r"), array("\n", "\n", "\n"), $value) . "'";
        }

        $origValue = $value;

        if (is_object($value)) {
            if ($value instanceof ProphecyInterface) {
                return sprintf('%s Object (*Prophecy*)', get_class($value));
            } elseif (in_array($value, $processedObjects, true)) {
                return sprintf('%s Object (*RECURSION*)', get_class($value));
            }

            $processedObjects[] = $value;

            // Convert object to array
            $value = self::toArray($value);
        }

        if (is_array($value)) {
            $whitespace = str_repeat('    ', $indentation);

            // There seems to be no other way to check arrays for recursion
            // http://www.php.net/manual/en/language.types.array.php#73936
            preg_match_all('/\n            \[(\w+)\] => Array\s+\*RECURSION\*/', print_r($value, true), $matches);
            $recursiveKeys = array_unique($matches[1]);

            // Convert to valid array keys
            // Numeric integer strings are automatically converted to integers
            // by PHP
            foreach ($recursiveKeys as $key => $recursiveKey) {
                if ((string) (integer) $recursiveKey === $recursiveKey) {
                    $recursiveKeys[$key] = (integer) $recursiveKey;
                }
            }

            $content = '';

            foreach ($value as $key => $val) {
                if (in_array($key, $recursiveKeys, true)) {
                    $val = 'Array (*RECURSION*)';
                } else {
                    $val = self::recursiveExport($val, $indentation + 1, $processedObjects);
                }

                $content .= $whitespace . '    ' . self::export($key) . ' => ' . $val . "\n";
            }

            if (strlen($content) > 0) {
                $content = "\n" . $content . $whitespace;
            }

            return sprintf(
                "%s (%s)",
                is_object($origValue) ? sprintf('%s:%s', get_class($origValue), spl_object_hash($origValue)) . ' Object' : 'Array', $content
            );
        }

        if (is_double($value) && (double)(integer) $value === $value) {
            return $value . '.0';
        }

        return (string) $value;
    }
}
