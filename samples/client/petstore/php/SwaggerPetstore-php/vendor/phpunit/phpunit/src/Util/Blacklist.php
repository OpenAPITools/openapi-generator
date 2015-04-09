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
 * Utility class for blacklisting PHPUnit's own source code files.
 *
 * @package    PHPUnit
 * @subpackage Util
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 4.0.0
 */
class PHPUnit_Util_Blacklist
{
    /**
     * @var array
     */
    public static $blacklistedClassNames = array(
        'File_Iterator' => 1,
        'PHP_CodeCoverage' => 1,
        'PHP_Invoker' => 1,
        'PHP_Timer' => 1,
        'PHP_Token' => 1,
        'PHPUnit_Framework_TestCase' => 2,
        'PHPUnit_Extensions_Database_TestCase' => 2,
        'PHPUnit_Framework_MockObject_Generator' => 2,
        'PHPUnit_Extensions_SeleniumTestCase' => 2,
        'PHPUnit_Extensions_Story_TestCase' => 2,
        'Text_Template' => 1,
        'Symfony\Component\Yaml\Yaml' => 1,
        'SebastianBergmann\Diff\Diff' => 1,
        'SebastianBergmann\Environment\Runtime' => 1,
        'SebastianBergmann\Comparator\Comparator' => 1,
        'SebastianBergmann\Exporter\Exporter' => 1,
        'SebastianBergmann\RecursionContext\Context' => 1,
        'SebastianBergmann\Version' => 1,
        'Composer\Autoload\ClassLoader' => 1,
        'Doctrine\Instantiator\Instantiator' => 1
    );

    /**
     * @var array
     */
    private static $directories;

    /**
     * @return array
     * @since  Method available since Release 4.1.0
     */
    public function getBlacklistedDirectories()
    {
        $this->initialize();

        return self::$directories;
    }

    /**
     * @param  string  $file
     * @return boolean
     */
    public function isBlacklisted($file)
    {
        if (defined('PHPUNIT_TESTSUITE')) {
            return false;
        }

        $this->initialize();

        foreach (self::$directories as $directory) {
            if (strpos($file, $directory) === 0) {
                return true;
            }
        }

        return false;
    }

    private function initialize()
    {
        if (self::$directories === null) {
            self::$directories = array();

            foreach (self::$blacklistedClassNames as $className => $parent) {
                if (!class_exists($className)) {
                    continue;
                }

                $reflector = new ReflectionClass($className);
                $directory = $reflector->getFileName();

                for ($i = 0; $i < $parent; $i++) {
                    $directory = dirname($directory);
                }

                self::$directories[] = $directory;
            }

            // Hide process isolation workaround on Windows.
            // @see PHPUnit_Util_PHP::factory()
            // @see PHPUnit_Util_PHP_Windows::process()
            if (DIRECTORY_SEPARATOR === '\\') {
                // tempnam() prefix is limited to first 3 chars.
                // @see http://php.net/manual/en/function.tempnam.php
                self::$directories[] = sys_get_temp_dir() . '\\PHP';
            }
        }
    }
}
