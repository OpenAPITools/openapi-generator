<?php
/*
 * This file is part of the PHPUnit_MockObject package.
 *
 * (c) Sebastian Bergmann <sebastian@phpunit.de>
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

use Doctrine\Instantiator\Instantiator;
use Doctrine\Instantiator\Exception\InvalidArgumentException as InstantiatorInvalidArgumentException;
use Doctrine\Instantiator\Exception\UnexpectedValueException as InstantiatorUnexpectedValueException;

if (!function_exists('trait_exists')) {
    function trait_exists($traitname, $autoload = true)
    {
        return false;
    }
}

/**
 * Mock Object Code Generator
 *
 * @package    PHPUnit_MockObject
 * @author     Sebastian Bergmann <sebastian@phpunit.de>
 * @copyright  Sebastian Bergmann <sebastian@phpunit.de>
 * @license    http://www.opensource.org/licenses/BSD-3-Clause  The BSD 3-Clause License
 * @version    Release: @package_version@
 * @link       http://github.com/sebastianbergmann/phpunit-mock-objects
 * @since      Class available since Release 1.0.0
 */
class PHPUnit_Framework_MockObject_Generator
{
    /**
     * @var array
     */
    private static $cache = array();

    /**
     * @var array
     */
    protected $blacklistedMethodNames = array(
      '__CLASS__' => TRUE,
      '__DIR__' => TRUE,
      '__FILE__' => TRUE,
      '__FUNCTION__' => TRUE,
      '__LINE__' => TRUE,
      '__METHOD__' => TRUE,
      '__NAMESPACE__' => TRUE,
      '__TRAIT__' => TRUE,
      '__clone' => TRUE,
      '__halt_compiler' => TRUE,
      'abstract' => TRUE,
      'and' => TRUE,
      'array' => TRUE,
      'as' => TRUE,
      'break' => TRUE,
      'callable' => TRUE,
      'case' => TRUE,
      'catch' => TRUE,
      'class' => TRUE,
      'clone' => TRUE,
      'const' => TRUE,
      'continue' => TRUE,
      'declare' => TRUE,
      'default' => TRUE,
      'die' => TRUE,
      'do' => TRUE,
      'echo' => TRUE,
      'else' => TRUE,
      'elseif' => TRUE,
      'empty' => TRUE,
      'enddeclare' => TRUE,
      'endfor' => TRUE,
      'endforeach' => TRUE,
      'endif' => TRUE,
      'endswitch' => TRUE,
      'endwhile' => TRUE,
      'eval' => TRUE,
      'exit' => TRUE,
      'expects' => TRUE,
      'extends' => TRUE,
      'final' => TRUE,
      'for' => TRUE,
      'foreach' => TRUE,
      'function' => TRUE,
      'global' => TRUE,
      'goto' => TRUE,
      'if' => TRUE,
      'implements' => TRUE,
      'include' => TRUE,
      'include_once' => TRUE,
      'instanceof' => TRUE,
      'insteadof' => TRUE,
      'interface' => TRUE,
      'isset' => TRUE,
      'list' => TRUE,
      'namespace' => TRUE,
      'new' => TRUE,
      'or' => TRUE,
      'print' => TRUE,
      'private' => TRUE,
      'protected' => TRUE,
      'public' => TRUE,
      'require' => TRUE,
      'require_once' => TRUE,
      'return' => TRUE,
      'static' => TRUE,
      'switch' => TRUE,
      'throw' => TRUE,
      'trait' => TRUE,
      'try' => TRUE,
      'unset' => TRUE,
      'use' => TRUE,
      'var' => TRUE,
      'while' => TRUE,
      'xor' => TRUE
    );

    /**
     * @var boolean
     */
    protected $soapLoaded = NULL;

    /**
     * Returns a mock object for the specified class.
     *
     * @param  array|string                $type
     * @param  array                       $methods
     * @param  array                       $arguments
     * @param  string                      $mockClassName
     * @param  boolean                     $callOriginalConstructor
     * @param  boolean                     $callOriginalClone
     * @param  boolean                     $callAutoload
     * @param  boolean                     $cloneArguments
     * @param  boolean                     $callOriginalMethods
     * @param  object                      $proxyTarget
     * @return object
     * @throws InvalidArgumentException
     * @throws PHPUnit_Framework_Exception
     * @throws PHPUnit_Framework_MockObject_RuntimeException
     * @since  Method available since Release 1.0.0
     */
    public function getMock($type, $methods = array(), array $arguments = array(), $mockClassName = '', $callOriginalConstructor = TRUE, $callOriginalClone = TRUE, $callAutoload = TRUE, $cloneArguments = TRUE, $callOriginalMethods = FALSE, $proxyTarget = NULL)
    {
        if (!is_array($type) && !is_string($type)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(1, 'array or string');
        }

        if (!is_string($mockClassName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(4, 'string');
        }

        if (!is_array($methods) && !is_null($methods)) {
            throw new InvalidArgumentException;
        }

        if ($type === 'Traversable' || $type === '\\Traversable') {
            $type = 'Iterator';
        }

        if (is_array($type)) {
            $type = array_unique(array_map(
              function ($type) {
                  if ($type === 'Traversable' ||
                      $type === '\\Traversable' ||
                      $type === '\\Iterator') {
                      return 'Iterator';
                  }

                  return $type;
              },
              $type
            ));
        }

        if (NULL !== $methods) {
            foreach ($methods as $method) {
                if (!preg_match('~[a-zA-Z_\x7f-\xff][a-zA-Z0-9_\x7f-\xff]*~', $method)) {
                    throw new PHPUnit_Framework_Exception(
                      sprintf(
                        'Cannot stub or mock method with invalid name "%s"',
                        $method
                      )
                    );
                }
            }

            if ($methods != array_unique($methods)) {
                throw new PHPUnit_Framework_MockObject_RuntimeException(
                  sprintf(
                    'Cannot stub or mock using a method list that contains duplicates: "%s"',
                    implode(', ', $methods)
                  )
                );
            }
        }

        if ($mockClassName != '' && class_exists($mockClassName, FALSE)) {
            $reflect = new ReflectionClass($mockClassName);

            if (!$reflect->implementsInterface("PHPUnit_Framework_MockObject_MockObject")) {
                throw new PHPUnit_Framework_MockObject_RuntimeException(
                  sprintf(
                    'Class "%s" already exists.',
                    $mockClassName
                  )
                );
            }
        }

        $mock = $this->generate(
          $type,
          $methods,
          $mockClassName,
          $callOriginalClone,
          $callAutoload,
          $cloneArguments,
          $callOriginalMethods
        );

        return $this->getObject(
          $mock['code'],
          $mock['mockClassName'],
          $type,
          $callOriginalConstructor,
          $callAutoload,
          $arguments,
          $callOriginalMethods,
          $proxyTarget
        );
    }

    /**
     * @param  string       $code
     * @param  string       $className
     * @param  array|string $type
     * @param  boolean      $callOriginalConstructor
     * @param  boolean      $callAutoload
     * @param  array        $arguments
     * @param  boolean      $callOriginalMethods
     * @param  object       $proxyTarget
     * @return object
     */
    protected function getObject($code, $className, $type = '', $callOriginalConstructor = FALSE, $callAutoload = FALSE, array $arguments = array(), $callOriginalMethods = FALSE, $proxyTarget = NULL)
    {
        $this->evalClass($code, $className);

        if ($callOriginalConstructor &&
            is_string($type) &&
            !interface_exists($type, $callAutoload)) {
            if (count($arguments) == 0) {
                $object = new $className;
            } else {
                $class  = new ReflectionClass($className);
                $object = $class->newInstanceArgs($arguments);
            }
        } else {
            try {
                $instantiator = new Instantiator;
                $object       = $instantiator->instantiate($className);
            } catch (InstantiatorUnexpectedValueException $exception) {
                if($exception->getPrevious()) {
                    $exception = $exception->getPrevious();
                }

                throw new PHPUnit_Framework_MockObject_RuntimeException(
                  $exception->getMessage()
                );
            } catch (InstantiatorInvalidArgumentException $exception) {
                throw new PHPUnit_Framework_MockObject_RuntimeException(
                  $exception->getMessage()
                );
            }
        }

        if ($callOriginalMethods) {
            if (!is_object($proxyTarget)) {
                if (count($arguments) == 0) {
                    $proxyTarget = new $type;
                } else {
                    $class       = new ReflectionClass($type);
                    $proxyTarget = $class->newInstanceArgs($arguments);
                }
            }

            $object->__phpunit_setOriginalObject($proxyTarget);
        }

        return $object;
    }

    /**
     * @param string $code
     * @param string $className
     */
    protected function evalClass($code, $className)
    {
        if (!class_exists($className, FALSE)) {
            eval($code);
        }
    }

    /**
     * Returns a mock object for the specified abstract class with all abstract
     * methods of the class mocked. Concrete methods to mock can be specified with
     * the last parameter
     *
     * @param  string                      $originalClassName
     * @param  array                       $arguments
     * @param  string                      $mockClassName
     * @param  boolean                     $callOriginalConstructor
     * @param  boolean                     $callOriginalClone
     * @param  boolean                     $callAutoload
     * @param  array                       $mockedMethods
     * @param  boolean                     $cloneArguments
     * @return object
     * @since  Method available since Release 1.0.0
     * @throws PHPUnit_Framework_MockObject_RuntimeException
     * @throws PHPUnit_Framework_Exception
     */
    public function getMockForAbstractClass($originalClassName, array $arguments = array(), $mockClassName = '', $callOriginalConstructor = TRUE, $callOriginalClone = TRUE, $callAutoload = TRUE, $mockedMethods = array(), $cloneArguments = TRUE)
    {
        if (!is_string($originalClassName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(1, 'string');
        }

        if (!is_string($mockClassName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(3, 'string');
        }

        if (class_exists($originalClassName, $callAutoload) ||
            interface_exists($originalClassName, $callAutoload)) {
            $reflector = new ReflectionClass($originalClassName);
            $methods   = $mockedMethods;

            foreach ($reflector->getMethods(ReflectionMethod::IS_ABSTRACT) as $method) {
                if (!in_array($method->getName(), $methods)) {
                    $methods[] = $method->getName();
                }
            }

            if (empty($methods)) {
                $methods = NULL;
            }

            return $this->getMock(
              $originalClassName,
              $methods,
              $arguments,
              $mockClassName,
              $callOriginalConstructor,
              $callOriginalClone,
              $callAutoload,
              $cloneArguments
            );
        } else {
            throw new PHPUnit_Framework_MockObject_RuntimeException(
              sprintf('Class "%s" does not exist.', $originalClassName)
            );
        }
    }

    /**
     * Returns a mock object for the specified trait with all abstract methods
     * of the trait mocked. Concrete methods to mock can be specified with the
     * `$mockedMethods` parameter.
     *
     * @param  string                   $traitName
     * @param  array                    $arguments
     * @param  string                   $mockClassName
     * @param  boolean                  $callOriginalConstructor
     * @param  boolean                  $callOriginalClone
     * @param  boolean                  $callAutoload
     * @param  array                    $mockedMethods
     * @param  boolean                  $cloneArguments
     * @return object
     * @since  Method available since Release 1.2.3
     * @throws PHPUnit_Framework_MockObject_RuntimeException
     * @throws PHPUnit_Framework_Exception
     */
    public function getMockForTrait($traitName, array $arguments = array(), $mockClassName = '', $callOriginalConstructor = TRUE, $callOriginalClone = TRUE, $callAutoload = TRUE, $mockedMethods = array(), $cloneArguments = TRUE)
    {
        if (!is_string($traitName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(1, 'string');
        }

        if (!is_string($mockClassName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(3, 'string');
        }

        if (!trait_exists($traitName, $callAutoload)) {
            throw new PHPUnit_Framework_MockObject_RuntimeException(
              sprintf(
                'Trait "%s" does not exist.',
                $traitName
              )
            );
        }

        $className = $this->generateClassName(
          $traitName, '', 'Trait_'
        );

        $templateDir   = dirname(__FILE__) . DIRECTORY_SEPARATOR . 'Generator' .
                         DIRECTORY_SEPARATOR;
        $classTemplate = new Text_Template(
                           $templateDir . 'trait_class.tpl'
                         );

        $classTemplate->setVar(
          array(
            'prologue'   => 'abstract ',
            'class_name' => $className['className'],
            'trait_name' => $traitName
          )
        );

        $this->evalClass(
          $classTemplate->render(),
          $className['className']
        );

        return $this->getMockForAbstractClass($className['className'], $arguments, $mockClassName, $callOriginalConstructor, $callOriginalClone, $callAutoload, $mockedMethods, $cloneArguments);
    }

    /**
     * Returns an object for the specified trait.
     *
     * @param  string                      $traitName
     * @param  array                       $arguments
     * @param  string                      $traitClassName
     * @param  boolean                     $callOriginalConstructor
     * @param  boolean                     $callOriginalClone
     * @param  boolean                     $callAutoload
     * @return object
     * @since  Method available since Release 1.1.0
     * @throws PHPUnit_Framework_MockObject_RuntimeException
     * @throws PHPUnit_Framework_Exception
     */
    public function getObjectForTrait($traitName, array $arguments = array(), $traitClassName = '', $callOriginalConstructor = TRUE, $callOriginalClone = TRUE, $callAutoload = TRUE)
    {
        if (!is_string($traitName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(1, 'string');
        }

        if (!is_string($traitClassName)) {
            throw PHPUnit_Util_InvalidArgumentHelper::factory(3, 'string');
        }

        if (!trait_exists($traitName, $callAutoload)) {
            throw new PHPUnit_Framework_MockObject_RuntimeException(
              sprintf(
                'Trait "%s" does not exist.',
                $traitName
              )
            );
        }

        $className = $this->generateClassName(
          $traitName, $traitClassName, 'Trait_'
        );

        $templateDir   = dirname(__FILE__) . DIRECTORY_SEPARATOR . 'Generator' .
                         DIRECTORY_SEPARATOR;
        $classTemplate = new Text_Template(
                           $templateDir . 'trait_class.tpl'
                         );

        $classTemplate->setVar(
          array(
            'prologue'   => '',
            'class_name' => $className['className'],
            'trait_name' => $traitName
          )
        );

        return $this->getObject(
          $classTemplate->render(),
          $className['className']
        );
    }

    /**
     * @param  array|string $type
     * @param  array        $methods
     * @param  string       $mockClassName
     * @param  boolean      $callOriginalClone
     * @param  boolean      $callAutoload
     * @param  boolean      $cloneArguments
     * @param  boolean      $callOriginalMethods
     * @return array
     */
    public function generate($type, array $methods = NULL, $mockClassName = '', $callOriginalClone = TRUE, $callAutoload = TRUE, $cloneArguments = TRUE, $callOriginalMethods = FALSE)
    {
        if (is_array($type)) {
            sort($type);
        }

        if ($mockClassName == '') {
            $key = md5(
              is_array($type) ? join('_', $type) : $type .
              serialize($methods) .
              serialize($callOriginalClone) .
              serialize($cloneArguments) .
              serialize($callOriginalMethods)
            );

            if (isset(self::$cache[$key])) {
                return self::$cache[$key];
            }
        }

        $mock = $this->generateMock(
          $type,
          $methods,
          $mockClassName,
          $callOriginalClone,
          $callAutoload,
          $cloneArguments,
          $callOriginalMethods
        );

        if (isset($key)) {
            self::$cache[$key] = $mock;
        }

        return $mock;
    }

    /**
     * @param  string                      $wsdlFile
     * @param  string                      $className
     * @param  array                       $methods
     * @param  array                       $options
     * @return string
     * @throws PHPUnit_Framework_MockObject_RuntimeException
     */
    public function generateClassFromWsdl($wsdlFile, $className, array $methods = array(), array $options = array())
    {
        if ($this->soapLoaded === NULL) {
            $this->soapLoaded = extension_loaded('soap');
        }

        if ($this->soapLoaded) {
            $options = array_merge($options, array('cache_wsdl' => WSDL_CACHE_NONE));
            $client   = new SoapClient($wsdlFile, $options);
            $_methods = array_unique($client->__getFunctions());
            unset($client);

            sort($_methods);

            $templateDir    = dirname(__FILE__) . DIRECTORY_SEPARATOR .
                              'Generator' . DIRECTORY_SEPARATOR;
            $methodTemplate = new Text_Template(
                                $templateDir . 'wsdl_method.tpl'
                              );
            $methodsBuffer  = '';

            foreach ($_methods as $method) {
                $nameStart = strpos($method, ' ') + 1;
                $nameEnd   = strpos($method, '(');
                $name      = substr($method, $nameStart, $nameEnd - $nameStart);

                if (empty($methods) || in_array($name, $methods)) {
                    $args    = explode(
                                 ',',
                                 substr(
                                   $method,
                                   $nameEnd + 1,
                                   strpos($method, ')') - $nameEnd - 1
                                 )
                               );
                    $numArgs = count($args);

                    for ($i = 0; $i < $numArgs; $i++) {
                        $args[$i] = substr($args[$i], strpos($args[$i], '$'));
                    }

                    $methodTemplate->setVar(
                      array(
                        'method_name' => $name,
                        'arguments'   => join(', ', $args)
                      )
                    );

                    $methodsBuffer .= $methodTemplate->render();
                }
            }

            $optionsBuffer = 'array(';

            foreach ($options as $key => $value) {
                $optionsBuffer .= $key . ' => ' . $value;
            }

            $optionsBuffer .= ')';

            $classTemplate = new Text_Template(
              $templateDir . 'wsdl_class.tpl'
            );

            $namespace = '';

            if (strpos($className, '\\') !== FALSE) {
                $parts     = explode('\\', $className);
                $className = array_pop($parts);
                $namespace = 'namespace ' . join('\\', $parts) . ';' . "\n\n";
            }

            $classTemplate->setVar(
              array(
                'namespace'  => $namespace,
                'class_name' => $className,
                'wsdl'       => $wsdlFile,
                'options'    => $optionsBuffer,
                'methods'    => $methodsBuffer
              )
            );

            return $classTemplate->render();
        } else {
            throw new PHPUnit_Framework_MockObject_RuntimeException(
              'The SOAP extension is required to generate a mock object ' .
              'from WSDL.'
            );
        }
    }

    /**
     * @param  array|string                $type
     * @param  array|null                  $methods
     * @param  string                      $mockClassName
     * @param  boolean                     $callOriginalClone
     * @param  boolean                     $callAutoload
     * @param  boolean                     $cloneArguments
     * @param  boolean                     $callOriginalMethods
     * @return array
     * @throws PHPUnit_Framework_Exception
     */
    protected function generateMock($type, $methods, $mockClassName, $callOriginalClone, $callAutoload, $cloneArguments, $callOriginalMethods)
    {
        $templateDir   = dirname(__FILE__) . DIRECTORY_SEPARATOR . 'Generator' .
                         DIRECTORY_SEPARATOR;
        $classTemplate = new Text_Template(
                           $templateDir . 'mocked_class.tpl'
                         );

        $additionalInterfaces = array();
        $cloneTemplate        = '';
        $isClass              = FALSE;
        $isInterface          = FALSE;

        $mockClassName = $this->generateClassName(
          $type, $mockClassName, 'Mock_'
        );

        if (is_array($type)) {
            foreach ($type as $_type) {
                if (!interface_exists($_type, $callAutoload)) {
                    throw new PHPUnit_Framework_Exception(
                      sprintf(
                        'Interface "%s" does not exist.', $_type
                      )
                    );
                }

                $additionalInterfaces[] = $_type;

                foreach (get_class_methods($_type) as $method) {
                    if (in_array($method, $methods)) {
                        throw new PHPUnit_Framework_Exception(
                          sprintf(
                            'Duplicate method "%s" not allowed.', $method
                          )
                        );
                    }

                    $methods[] = $method;
                }
            }
        }

        if (class_exists($mockClassName['fullClassName'], $callAutoload)) {
            $isClass = TRUE;
        } else {
            if (interface_exists($mockClassName['fullClassName'], $callAutoload)) {
                $isInterface = TRUE;
            }
        }

        if (!class_exists($mockClassName['fullClassName'], $callAutoload) &&
            !interface_exists($mockClassName['fullClassName'], $callAutoload)) {
            $prologue = 'class ' . $mockClassName['originalClassName'] . "\n{\n}\n\n";

            if (!empty($mockClassName['namespaceName'])) {
                $prologue = 'namespace ' . $mockClassName['namespaceName'] .
                            " {\n\n" . $prologue . "}\n\n" .
                            "namespace {\n\n";

                $epilogue = "\n\n}";
            }

            $cloneTemplate = new Text_Template(
              $templateDir . 'mocked_clone.tpl'
            );
        } else {
            $class = new ReflectionClass($mockClassName['fullClassName']);

            if ($class->isFinal()) {
                throw new PHPUnit_Framework_Exception(
                  sprintf(
                    'Class "%s" is declared "final" and cannot be mocked.',
                    $mockClassName['fullClassName']
                  )
                );
            }

            if ($class->hasMethod('__clone')) {
                $cloneMethod = $class->getMethod('__clone');

                if (!$cloneMethod->isFinal()) {
                    if ($callOriginalClone && !$isInterface) {
                        $cloneTemplate = new Text_Template(
                          $templateDir . 'unmocked_clone.tpl'
                        );
                    } else {
                        $cloneTemplate = new Text_Template(
                          $templateDir . 'mocked_clone.tpl'
                        );
                    }
                }
            } else {
                $cloneTemplate = new Text_Template(
                  $templateDir . 'mocked_clone.tpl'
                );
            }
        }

        if (is_object($cloneTemplate)) {
            $cloneTemplate = $cloneTemplate->render();
        }

        if (is_array($methods) && empty($methods) &&
            ($isClass || $isInterface)) {
            $methods = get_class_methods($mockClassName['fullClassName']);
        }

        if (!is_array($methods)) {
            $methods = array();
        }

        $mockedMethods = '';

        if (isset($class)) {
            // https://github.com/sebastianbergmann/phpunit-mock-objects/issues/103
            if ($isInterface && $class->implementsInterface('Traversable') &&
                !$class->implementsInterface('Iterator') &&
                !$class->implementsInterface('IteratorAggregate')) {
                $additionalInterfaces[] = 'Iterator';
                $methods = array_merge($methods, get_class_methods('Iterator'));
            }

            foreach ($methods as $methodName) {
                try {
                    $method = $class->getMethod($methodName);

                    if ($this->canMockMethod($method)) {
                        $mockedMethods .= $this->generateMockedMethodDefinitionFromExisting(
                          $templateDir,
                          $method,
                          $cloneArguments,
                          $callOriginalMethods
                        );
                    }
                } catch (ReflectionException $e) {
                    $mockedMethods .= $this->generateMockedMethodDefinition(
                      $templateDir, $mockClassName['fullClassName'], $methodName, $cloneArguments
                    );
                }
            }
        } else {
            foreach ($methods as $methodName) {
                $mockedMethods .= $this->generateMockedMethodDefinition(
                  $templateDir, $mockClassName['fullClassName'], $methodName, $cloneArguments
                );
            }
        }

        $method = '';

        if (!in_array('method', $methods)) {
            $methodTemplate = new Text_Template(
                $templateDir . 'mocked_class_method.tpl'
            );

            $method = $methodTemplate->render();
        }

        $classTemplate->setVar(
          array(
            'prologue'          => isset($prologue) ? $prologue : '',
            'epilogue'          => isset($epilogue) ? $epilogue : '',
            'class_declaration' => $this->generateMockClassDeclaration(
                                     $mockClassName,
                                     $isInterface,
                                     $additionalInterfaces
                                   ),
            'clone'             => $cloneTemplate,
            'mock_class_name'   => $mockClassName['className'],
            'mocked_methods'    => $mockedMethods,
            'method'            => $method
          )
        );

        return array(
          'code'          => $classTemplate->render(),
          'mockClassName' => $mockClassName['className']
        );
    }

    /**
     * @param  array|string $type
     * @param  string       $className
     * @param  string       $prefix
     * @return array
     */
    protected function generateClassName($type, $className, $prefix)
    {
        if (is_array($type)) {
            $type = join('_', $type);
        }

        if ($type[0] == '\\') {
            $type = substr($type, 1);
        }

        $classNameParts = explode('\\', $type);

        if (count($classNameParts) > 1) {
            $type          = array_pop($classNameParts);
            $namespaceName = join('\\', $classNameParts);
            $fullClassName = $namespaceName . '\\' . $type;
        } else {
            $namespaceName = '';
            $fullClassName = $type;
        }

        if ($className == '') {
            do {
                $className = $prefix . $type . '_' .
                             substr(md5(microtime()), 0, 8);
            } while (class_exists($className, FALSE));
        }

        return array(
          'className'         => $className,
          'originalClassName' => $type,
          'fullClassName'     => $fullClassName,
          'namespaceName'     => $namespaceName
        );
    }

    /**
     * @param  array   $mockClassName
     * @param  boolean $isInterface
     * @param  array   $additionalInterfaces
     * @return array
     */
    protected function generateMockClassDeclaration(array $mockClassName, $isInterface, array $additionalInterfaces = array())
    {
        $buffer = 'class ';

        $additionalInterfaces[] = 'PHPUnit_Framework_MockObject_MockObject';
        $interfaces = implode(', ', $additionalInterfaces);

        if ($isInterface) {
            $buffer .= sprintf(
              "%s implements %s",
              $mockClassName['className'],
              $interfaces
            );

            if (!in_array($mockClassName['originalClassName'], $additionalInterfaces)) {
                $buffer .= ', ';

                if (!empty($mockClassName['namespaceName'])) {
                    $buffer .= $mockClassName['namespaceName'] . '\\';
                }

                $buffer .= $mockClassName['originalClassName'];
            }
        } else {
            $buffer .= sprintf(
              "%s extends %s%s implements %s",
              $mockClassName['className'],
              !empty($mockClassName['namespaceName']) ? $mockClassName['namespaceName'] . '\\' : '',
              $mockClassName['originalClassName'],
              $interfaces
            );
        }

        return $buffer;
    }

    /**
     * @param  string           $templateDir
     * @param  ReflectionMethod $method
     * @param  boolean          $cloneArguments
     * @param  boolean          $callOriginalMethods
     * @return string
     */
    protected function generateMockedMethodDefinitionFromExisting($templateDir, ReflectionMethod $method, $cloneArguments, $callOriginalMethods)
    {
        if ($method->isPrivate()) {
            $modifier = 'private';
        } elseif ($method->isProtected()) {
            $modifier = 'protected';
        } else {
            $modifier = 'public';
        }

        if ($method->isStatic()) {
            $modifier .= ' static';
        }

        if ($method->returnsReference()) {
            $reference = '&';
        } else {
            $reference = '';
        }

        return $this->generateMockedMethodDefinition(
          $templateDir,
          $method->getDeclaringClass()->getName(),
          $method->getName(),
          $cloneArguments,
          $modifier,
          $this->getMethodParameters($method),
          $this->getMethodParameters($method, TRUE),
          $reference,
          $callOriginalMethods,
          $method->isStatic()
        );
    }

    /**
     * @param  string  $templateDir
     * @param  string  $className
     * @param  string  $methodName
     * @param  boolean $cloneArguments
     * @param  string  $modifier
     * @param  string  $arguments_decl
     * @param  string  $arguments_call
     * @param  string  $reference
     * @param  boolean $callOriginalMethods
     * @param  boolean $static
     * @return string
     */
    protected function generateMockedMethodDefinition($templateDir, $className, $methodName, $cloneArguments = TRUE, $modifier = 'public', $arguments_decl = '', $arguments_call = '', $reference = '', $callOriginalMethods = FALSE, $static = FALSE)
    {
        if ($static) {
            $templateFile = 'mocked_static_method.tpl';
        } else {
            $templateFile = sprintf(
              '%s_method.tpl',
              $callOriginalMethods ? 'proxied' : 'mocked'
            );
        }

        $template = new Text_Template($templateDir . $templateFile);

        $template->setVar(
          array(
            'arguments_decl'  => $arguments_decl,
            'arguments_call'  => $arguments_call,
            'arguments_count' => !empty($arguments_call) ? count(explode(',', $arguments_call)) : 0,
            'class_name'      => $className,
            'method_name'     => $methodName,
            'modifier'        => $modifier,
            'reference'       => $reference,
            'clone_arguments' => $cloneArguments ? 'TRUE' : 'FALSE'
          )
        );

        return $template->render();
    }

    /**
     * @param  ReflectionMethod $method
     * @return boolean
     */
    protected function canMockMethod(ReflectionMethod $method)
    {
        if ($method->isConstructor() ||
            $method->isFinal() ||
            isset($this->blacklistedMethodNames[$method->getName()])) {
            return FALSE;
        }

        return TRUE;
    }

    /**
     * Returns the parameters of a function or method.
     *
     * @param  ReflectionMethod $method
     * @param  boolean          $forCall
     * @return string
     * @throws PHPUnit_Framework_MockObject_RuntimeException
     * @since  Method available since Release 2.0.0
     */
    protected function getMethodParameters(ReflectionMethod $method, $forCall = FALSE)
    {
        $parameters = array();

        foreach ($method->getParameters() as $i => $parameter) {
            $name = '$' . $parameter->getName();

            /* Note: PHP extensions may use empty names for reference arguments
             * or "..." for methods taking a variable number of arguments.
             */
            if ($name === '$' || $name === '$...') {
                $name = '$arg' . $i;
            }

            if ($this->isVariadic($parameter)) {
                if ($forCall) {
                    continue;
                } else {
                    $name = '...' . $name;
                }
            }

            $default   = '';
            $reference = '';
            $typeHint  = '';

            if (!$forCall) {
                if ($parameter->isArray()) {
                    $typeHint = 'array ';
                } elseif ((defined('HHVM_VERSION') || version_compare(PHP_VERSION, '5.4.0', '>='))
                          && $parameter->isCallable()) {
                    $typeHint = 'callable ';
                } else {
                    try {
                        $class = $parameter->getClass();
                    } catch (ReflectionException $e) {
                        throw new PHPUnit_Framework_MockObject_RuntimeException(
                          sprintf(
                            'Cannot mock %s::%s() because a class or ' .
                            'interface used in the signature is not loaded',
                            $method->getDeclaringClass()->getName(),
                            $method->getName()
                          ),
                          0,
                          $e
                        );
                    }

                    if ($class !== NULL) {
                        $typeHint = $class->getName() . ' ';
                    }
                }

                if (!$this->isVariadic($parameter)) {
                    if ($parameter->isDefaultValueAvailable()) {
                        $value = $parameter->getDefaultValue();
                        $default = ' = ' . var_export($value, TRUE);
                    } elseif ($parameter->isOptional()) {
                        $default = ' = null';
                    }
                }
            }

            if ($parameter->isPassedByReference()) {
                $reference = '&';
            }

            $parameters[] = $typeHint . $reference . $name . $default;
        }

        return join(', ', $parameters);
    }

    /**
     * @param  ReflectionParameter $parameter
     * @return boolean
     * @since  Method available since Release 2.2.1
     */
    private function isVariadic(ReflectionParameter $parameter)
    {
        return method_exists('ReflectionParameter', 'isVariadic') && $parameter->isVariadic();
    }
}
