<?php
/*
 * This file is part of PHPUnit.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * An interface to define how a test suite should be loaded.
 *
 * @package    PHPUnit
 * @subpackage Runner
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Interface available since Release 2.0.0
 */
interface PHPUnit_Runner_TestSuiteLoader
{
    /**
     * @param  string          $suiteClassName
     * @param  string          $suiteClassFile
     * @return ReflectionClass
     */
    public function load($suiteClassName, $suiteClassFile = '');

    /**
     * @param  ReflectionClass $aClass
     * @return ReflectionClass
     */
    public function reload(ReflectionClass $aClass);
}
