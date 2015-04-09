<?php
/*
 * This file is part of the PHP_CodeCoverage package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Driver for Xdebug's code coverage functionality.
 *
 * @category   PHP
 * @package    CodeCoverage
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 1.0.0
 * @codeCoverageIgnore
 */
class PHP_CodeCoverage_Driver_Xdebug implements PHP_CodeCoverage_Driver
{
    /**
     * Constructor.
     */
    public function __construct()
    {
        if (!extension_loaded('xdebug')) {
            throw new PHP_CodeCoverage_Exception('This driver requires Xdebug');
        }

        if (version_compare(phpversion('xdebug'), '2.2.0-dev', '>=') &&
            !ini_get('xdebug.coverage_enable')) {
            throw new PHP_CodeCoverage_Exception(
                'xdebug.coverage_enable=On has to be set in php.ini'
            );
        }
    }

    /**
     * Start collection of code coverage information.
     */
    public function start()
    {
        xdebug_start_code_coverage(XDEBUG_CC_UNUSED | XDEBUG_CC_DEAD_CODE);
    }

    /**
     * Stop collection of code coverage information.
     *
     * @return array
     */
    public function stop()
    {
        $data = xdebug_get_code_coverage();
        xdebug_stop_code_coverage();

        return $this->cleanup($data);
    }

    /**
     * @param  array $data
     * @return array
     * @since Method available since Release 2.0.0
     */
    private function cleanup(array $data)
    {
        foreach (array_keys($data) as $file) {
            if (isset($data[$file][0])) {
                unset($data[$file][0]);
            }

            if (file_exists($file)) {
                $numLines = $this->getNumberOfLinesInFile($file);

                foreach (array_keys($data[$file]) as $line) {
                    if (isset($data[$file][$line]) && $line > $numLines) {
                        unset($data[$file][$line]);
                    }
                }
            }
        }

        return $data;
    }

    /**
     * @param  string $file
     * @return integer
     * @since Method available since Release 2.0.0
     */
    private function getNumberOfLinesInFile($file)
    {
        $buffer = file_get_contents($file);
        $lines  = substr_count($buffer, "\n");

        if (substr($buffer, -1) !== "\n") {
            $lines++;
        }

        return $lines;
    }
}
