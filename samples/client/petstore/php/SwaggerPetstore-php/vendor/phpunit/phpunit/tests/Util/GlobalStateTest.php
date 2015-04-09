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
 *
 *
 * @package    PHPUnit
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 */
class Util_GlobalStateTest extends PHPUnit_Framework_TestCase
{

    /**
     * @covers PHPUnit_Util_GlobalState::processIncludedFilesAsString
     */
    public function testIncludedFilesAsStringSkipsVfsProtocols()
    {
        $dir = __DIR__;
        $files = array(
            'phpunit', // The 0 index is not used
            $dir . '/ConfigurationTest.php',
            $dir . '/GlobalStateTest.php',
            'vfs://' . $dir . '/RegexTest.php',
            'phpvfs53e46260465c7://' . $dir . '/TestTest.php',
            'file://' . $dir . '/XMLTest.php'
        );

        $this->assertEquals(
            "require_once '" . $dir . "/ConfigurationTest.php';\n" .
            "require_once '" . $dir . "/GlobalStateTest.php';\n" .
            "require_once 'file://" . $dir . "/XMLTest.php';\n", PHPUnit_Util_GlobalState::processIncludedFilesAsString($files));
    }
}
