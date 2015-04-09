<?php
/*
 * This file is part of the PHPUnit_MockObject package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

/**
 * Stubs a method by raising a user-defined exception.
 *
 * @package    PHPUnit_MockObject
 * @author     Oliver Schlicht <o.schlicht@bitExpert.de>
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Stub_Exception implements PHPUnit_Framework_MockObject_Stub
{
    protected $exception;

    public function __construct(Exception $exception)
    {
        $this->exception = $exception;
    }

    public function invoke(PHPUnit_Framework_MockObject_Invocation $invocation)
    {
        throw $this->exception;
    }

    public function toString()
    {
        return sprintf(
          'raise user-specified exception %s',

          PHPUnit_Util_Type::export($this->exception)
        );
    }
}
