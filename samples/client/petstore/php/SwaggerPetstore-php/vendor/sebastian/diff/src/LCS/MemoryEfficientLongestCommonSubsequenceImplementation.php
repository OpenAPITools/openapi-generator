<?php
/*
 * This file is part of the Diff package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

namespace SebastianBergmann\Diff\LCS;

/**
 * Memory-efficient implementation of longest common subsequence calculation.
 *
 * @package    Diff
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @author     Denes Lados <lados.denes@gmail.com>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.github.com/sebastianbergmann/diff
 */
class MemoryEfficientImplementation implements LongestCommonSubsequence
{
    /**
     * Calculates the longest common subsequence of two arrays.
     *
     * @param  array $from
     * @param  array $to
     * @return array
     */
    public function calculate(array $from, array $to)
    {
        $cFrom = count($from);
        $cTo   = count($to);

        if ($cFrom == 0) {
            return array();
        } elseif ($cFrom == 1) {
            if (in_array($from[0], $to)) {
                return array($from[0]);
            } else {
                return array();
            }
        } else {
            $i         = intval($cFrom / 2);
            $fromStart = array_slice($from, 0, $i);
            $fromEnd   = array_slice($from, $i);
            $llB       = $this->length($fromStart, $to);
            $llE       = $this->length(array_reverse($fromEnd), array_reverse($to));
            $jMax      = 0;
            $max       = 0;

            for ($j = 0; $j <= $cTo; $j++) {
                $m = $llB[$j] + $llE[$cTo - $j];

                if ($m >= $max) {
                    $max  = $m;
                    $jMax = $j;
                }
            }

            $toStart = array_slice($to, 0, $jMax);
            $toEnd   = array_slice($to, $jMax);

            return array_merge(
                $this->calculate($fromStart, $toStart),
                $this->calculate($fromEnd, $toEnd)
            );
        }
    }

    /**
     * @param array $from
     * @param array $to
     * @return array
     */
    private function length(array $from, array $to)
    {
        $current = array_fill(0, count($to) + 1, 0);
        $cFrom   = count($from);
        $cTo     = count($to);

        for ($i = 0; $i < $cFrom; $i++) {
            $prev = $current;

            for ($j = 0; $j < $cTo; $j++) {
                if ($from[$i] == $to[$j]) {
                    $current[$j + 1] = $prev[$j] + 1;
                } else {
                    $current[$j + 1] = max($current[$j], $prev[$j + 1]);
                }
            }
        }

        return $current;
    }
}
