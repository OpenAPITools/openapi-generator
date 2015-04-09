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
 * Suite for .phpt test cases.
 *
 * @package    PHPUnit
 * @subpackage Extensions_PhptTestCase
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 3.1.4
 */
class PHPUnit_Extensions_PhptTestSuite extends PHPUnit_Framework_TestSuite
{
    /**
     * Constructs a new TestSuite for .phpt test cases.
     *
     * @param  string                      $directory
     * @param  array                       $options   Array with ini settings for the php instance run,
     *                                                key being the name if the setting, value the ini value.
     * @throws PHPUnit_Framework_Exception
     */
    public function __construct($directory, array $options = array())
    {
        if (is_string($directory) && is_dir($directory)) {
            $this->setName($directory);

            $facade = new File_Iterator_Facade;
            $files  = $facade->getFilesAsArray($directory, '.phpt');

            foreach ($files as $file) {
                $this->addTestFile($file, $options);
            }
        } else {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(1, 'directory name');
        }
    }
}
