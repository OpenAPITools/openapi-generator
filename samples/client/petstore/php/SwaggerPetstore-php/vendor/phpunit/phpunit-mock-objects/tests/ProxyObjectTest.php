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
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://www.phpunit.de/
 * @since      Class available since Release 2.0.0
 */
class Framework_ProxyObjectTest extends PHPUnit_Framework_TestCase
{
    public function testMockedMethodIsProxiedToOriginalMethod()
    {
        $proxy = $this->getMockBuilder('Bar')
                      ->enableProxyingToOriginalMethods()
                      ->getMock();

        $proxy->expects($this->once())
              ->method('doSomethingElse');

        $foo = new Foo;
        $this->assertEquals('result', $foo->doSomething($proxy));
    }

    public function testMockedMethodWithReferenceIsProxiedToOriginalMethod()
    {
        $proxy = $this->getMockBuilder('MethodCallbackByReference')
                      ->enableProxyingToOriginalMethods()
                      ->getMock();
        $a = $b = $c = 0;

        $proxy->callback($a, $b, $c);

        $this->assertEquals(1, $b);
    }
}
