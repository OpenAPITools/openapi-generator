<?php

namespace Guzzle\Tests\Http\Message;

use Guzzle\Common\Collection;
use Guzzle\Http\Message\Header\HeaderCollection;

/**
 * Class used to compare HTTP headers using a custom DSL
 */
class HeaderComparison
{
    /**
     * Compare HTTP headers and use special markup to filter values
     * A header prefixed with '!' means it must not exist
     * A header prefixed with '_' means it must be ignored
     * A header value of '*' means anything after the * will be ignored
     *
     * @param array $filteredHeaders Array of special headers
     * @param array $actualHeaders   Array of headers to check against
     *
     * @return array|bool Returns an array of the differences or FALSE if none
     */
    public function compare($filteredHeaders, $actualHeaders)
    {
        $expected = array();
        $ignore = array();
        $absent = array();

        if ($actualHeaders instanceof HeaderCollection) {
            $actualHeaders = $actualHeaders->toArray();
        }

        foreach ($filteredHeaders as $k => $v) {
            if ($k[0] == '_') {
                // This header should be ignored
                $ignore[] = str_replace('_', '', $k);
            } elseif ($k[0] == '!') {
                // This header must not be present
                $absent[] = str_replace('!', '', $k);
            } else {
                $expected[$k] = $v;
            }
        }

        return $this->compareArray($expected, $actualHeaders, $ignore, $absent);
    }

    /**
     * Check if an array of HTTP headers matches another array of HTTP headers while taking * into account as a wildcard
     *
     * @param array            $expected Expected HTTP headers (allows wildcard values)
     * @param array|Collection $actual   Actual HTTP header array
     * @param array            $ignore   Headers to ignore from the comparison
     * @param array            $absent   Array of headers that must not be present
     *
     * @return array|bool Returns an array of the differences or FALSE if none
     */
    public function compareArray(array $expected, $actual, array $ignore = array(), array $absent = array())
    {
        $differences = array();

        // Add information about headers that were present but weren't supposed to be
        foreach ($absent as $header) {
            if ($this->hasKey($header, $actual)) {
                $differences["++ {$header}"] = $actual[$header];
                unset($actual[$header]);
            }
        }

        // Check if expected headers are missing
        foreach ($expected as $header => $value) {
            if (!$this->hasKey($header, $actual)) {
                $differences["- {$header}"] = $value;
            }
        }

        // Flip the ignore array so it works with the case insensitive helper
        $ignore = array_flip($ignore);
        // Allow case-insensitive comparisons in wildcards
        $expected = array_change_key_case($expected);

        // Compare the expected and actual HTTP headers in no particular order
        foreach ($actual as $key => $value) {

            // If this is to be ignored, the skip it
            if ($this->hasKey($key, $ignore)) {
                continue;
            }

            // If the header was not expected
            if (!$this->hasKey($key, $expected)) {
                $differences["+ {$key}"] = $value;
                continue;
            }

            // Check values and take wildcards into account
            $lkey = strtolower($key);
            $pos = is_string($expected[$lkey]) ? strpos($expected[$lkey], '*') : false;

            foreach ((array) $actual[$key] as $v) {
                if (($pos === false && $v != $expected[$lkey]) || $pos > 0 && substr($v, 0, $pos) != substr($expected[$lkey], 0, $pos)) {
                    $differences[$key] = "{$value} != {$expected[$lkey]}";
                }
            }
        }

        return empty($differences) ? false : $differences;
    }

    /**
     * Case insensitive check if an array have a key
     *
     * @param string $key   Key to check
     * @param array  $array Array to check
     *
     * @return bool
     */
    protected function hasKey($key, $array)
    {
        if ($array instanceof Collection) {
            $keys = $array->getKeys();
        } else {
            $keys = array_keys($array);
        }

        foreach ($keys as $k) {
            if (!strcasecmp($k, $key)) {
                return true;
            }
        }

        return false;
    }
}
