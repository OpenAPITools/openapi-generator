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
 * Implementation of the Builder pattern for Mock objects.
 *
 * @package    PHPUnit_MockObject
 * @author     Giorgio Sironi <piccoloprincipeazzurro@gmail.com>
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      File available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_MockBuilder
{
    /**
     * @var PHPUnit_Framework_TestCase
     */
    private $testCase;

    /**
     * @var string
     */
    private $type;

    /**
     * @var array
     */
    private $methods = array();

    /**
     * @var string
     */
    private $mockClassName = '';

    /**
     * @var array
     */
    private $constructorArgs = array();

    /**
     * @var boolean
     */
    private $originalConstructor = TRUE;

    /**
     * @var boolean
     */
    private $originalClone = TRUE;

    /**
     * @var boolean
     */
    private $autoload = TRUE;

    /**
     * @var boolean
     */
    private $cloneArguments = FALSE;

    /**
     * @var boolean
     */
    private $callOriginalMethods = FALSE;

    /**
     * @var object
     */
    private $proxyTarget = NULL;

    /**
     * @param PHPUnit_Framework_TestCase $testCase
     * @param array|string               $type
     */
    public function __construct(PHPUnit_Framework_TestCase $testCase, $type)
    {
        $this->testCase = $testCase;
        $this->type     = $type;
    }

    /**
     * Creates a mock object using a fluent interface.
     *
     * @return PHPUnit_Framework_MockObject_MockObject
     */
    public function getMock()
    {
        return $this->testCase->getMock(
          $this->type,
          $this->methods,
          $this->constructorArgs,
          $this->mockClassName,
          $this->originalConstructor,
          $this->originalClone,
          $this->autoload,
          $this->cloneArguments,
          $this->callOriginalMethods,
          $this->proxyTarget
        );
    }

    /**
     * Creates a mock object for an abstract class using a fluent interface.
     *
     * @return PHPUnit_Framework_MockObject_MockObject
     */
    public function getMockForAbstractClass()
    {
        return $this->testCase->getMockForAbstractClass(
          $this->type,
          $this->constructorArgs,
          $this->mockClassName,
          $this->originalConstructor,
          $this->originalClone,
          $this->autoload,
          $this->methods,
          $this->cloneArguments
        );
    }

    /**
     * Creates a mock object for a trait using a fluent interface.
     *
     * @return PHPUnit_Framework_MockObject_MockObject
     */
    public function getMockForTrait()
    {
        return $this->testCase->getMockForTrait(
          $this->type,
          $this->constructorArgs,
          $this->mockClassName,
          $this->originalConstructor,
          $this->originalClone,
          $this->autoload,
          $this->methods,
          $this->cloneArguments
        );
    }

    /**
     * Specifies the subset of methods to mock. Default is to mock all of them.
     *
     * @param  array|null                               $methods
     * @return PHPUnit_Framework_MockObject_MockBuilder
     */
    public function setMethods($methods)
    {
        $this->methods = $methods;

        return $this;
    }

    /**
     * Specifies the arguments for the constructor.
     *
     * @param  array                                    $args
     * @return PHPUnit_Framework_MockObject_MockBuilder
     */
    public function setConstructorArgs(array $args)
    {
        $this->constructorArgs = $args;

        return $this;
    }

    /**
     * Specifies the name for the mock class.
     *
     * @param  string                                   $name
     * @return PHPUnit_Framework_MockObject_MockBuilder
     */
    public function setMockClassName($name)
    {
        $this->mockClassName = $name;

        return $this;
    }

    /**
     * Disables the invocation of the original constructor.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     */
    public function disableOriginalConstructor()
    {
        $this->originalConstructor = FALSE;

        return $this;
    }

    /**
     * Enables the invocation of the original constructor.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 1.2.0
     */
    public function enableOriginalConstructor()
    {
        $this->originalConstructor = TRUE;

        return $this;
    }

    /**
     * Disables the invocation of the original clone constructor.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     */
    public function disableOriginalClone()
    {
        $this->originalClone = FALSE;

        return $this;
    }

    /**
     * Enables the invocation of the original clone constructor.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 1.2.0
     */
    public function enableOriginalClone()
    {
        $this->originalClone = TRUE;

        return $this;
    }

    /**
     * Disables the use of class autoloading while creating the mock object.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     */
    public function disableAutoload()
    {
        $this->autoload = FALSE;

        return $this;
    }

    /**
     * Enables the use of class autoloading while creating the mock object.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 1.2.0
     */
    public function enableAutoload()
    {
        $this->autoload = TRUE;

        return $this;
    }

    /**
     * Disables the cloning of arguments passed to mocked methods.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 1.2.0
     */
    public function disableArgumentCloning()
    {
        $this->cloneArguments = FALSE;

        return $this;
    }

    /**
     * Enables the cloning of arguments passed to mocked methods.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 1.2.0
     */
    public function enableArgumentCloning()
    {
        $this->cloneArguments = TRUE;

        return $this;
    }

    /**
     * Enables the invocation of the original methods.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 2.0.0
     */
    public function enableProxyingToOriginalMethods()
    {
        $this->callOriginalMethods = TRUE;

        return $this;
    }

    /**
     * Disables the invocation of the original methods.
     *
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 2.0.0
     */
    public function disableProxyingToOriginalMethods()
    {
        $this->callOriginalMethods = FALSE;
        $this->proxyTarget         = NULL;

        return $this;
    }

    /**
     * Sets the proxy target.
     *
     * @param  object                                   $object
     * @return PHPUnit_Framework_MockObject_MockBuilder
     * @since  Method available since Release 2.0.0
     */
    public function setProxyTarget($object)
    {
        $this->proxyTarget = $object;

        return $this;
    }
}
