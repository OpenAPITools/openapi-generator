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
 * Driver for HHVM's code coverage functionality.
 *
 * @category   PHP
 * @package    CodeCoverage
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/php-code-coverage
 * @since      Class available since Release 1.3.0
 * @codeCoverageIgnore
 */
class PHP_CodeCoverage_Driver_HHVM implements PHP_CodeCoverage_Driver
{
    /**
     * Constructor.
     */
    public function __construct()
    {
        if (!defined('HHVM_VERSION')) {
            throw new PHP_CodeCoverage_Exception('This driver requires HHVM');
        }
    }

    /**
     * Start collection of code coverage information.
     */
    public function start()
    {
        fb_enable_code_coverage();
    }

    /**
     * Stop collection of code coverage information.
     *
     * @return array
     */
    public function stop()
    {
        $codeCoverage = fb_get_code_coverage(true);

        fb_disable_code_coverage();

        return $codeCoverage;
    }
}
