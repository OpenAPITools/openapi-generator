<?php

namespace spec\Prophecy\Doubler\Generator;

use PhpSpec\ObjectBehavior;
use I\Simply;

use ReflectionClass;
use ReflectionMethod;
use ReflectionParameter;

class ClassMirrorSpec extends ObjectBehavior
{
    /**
     * @param ReflectionClass  $class
     * @param ReflectionMethod $method1
     * @param ReflectionMethod $method2
     * @param ReflectionMethod $method3
     */
    function it_reflects_a_class_by_mirroring_all_its_public_methods(
        $class, $method1, $method2, $method3
    )
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array(
            $method1, $method2, $method3
        ));

        $method1->getName()->willReturn('getName');
        $method2->getName()->willReturn('isPublic');
        $method3->getName()->willReturn('isAbstract');

        $method1->isFinal()->willReturn(false);
        $method2->isFinal()->willReturn(false);
        $method3->isFinal()->willReturn(false);

        $method1->isProtected()->willReturn(false);
        $method2->isProtected()->willReturn(false);
        $method3->isProtected()->willReturn(false);

        $method1->isStatic()->willReturn(false);
        $method2->isStatic()->willReturn(false);
        $method3->isStatic()->willReturn(false);

        $method1->returnsReference()->willReturn(false);
        $method2->returnsReference()->willReturn(false);
        $method3->returnsReference()->willReturn(false);

        $method1->getParameters()->willReturn(array());
        $method2->getParameters()->willReturn(array());
        $method3->getParameters()->willReturn(array());

        $classNode   = $this->reflect($class, array());
        $classNode->shouldBeAnInstanceOf('Prophecy\Doubler\Generator\Node\ClassNode');
        $classNode->getParentClass()->shouldReturn('Custom\ClassName');

        $methodNodes = $classNode->getMethods();
        $methodNodes->shouldHaveCount(3);

        $classNode->hasMethod('getName')->shouldReturn(true);
        $classNode->hasMethod('isPublic')->shouldReturn(true);
        $classNode->hasMethod('isAbstract')->shouldReturn(true);
    }

    /**
     * @param ReflectionClass     $class
     * @param ReflectionMethod    $method
     * @param ReflectionParameter $parameter
     */
    function it_changes_argument_names_if_they_are_varying($class, $method, $parameter)
    {

        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method));
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());

        $method->getParameters()->willReturn(array($parameter));
        $method->getName()->willReturn('methodName');
        $method->isFinal()->willReturn(false);
        $method->isProtected()->willReturn(false);
        $method->isStatic()->willReturn(false);
        $method->returnsReference()->willReturn(false);

        $parameter->getName()->willReturn('...');
        $parameter->isDefaultValueAvailable()->willReturn(true);
        $parameter->getDefaultValue()->willReturn(null);
        $parameter->isPassedByReference()->willReturn(false);
        $parameter->getClass()->willReturn($class);

        $classNode = $this->reflect($class, array());

        $methodNodes = $classNode->getMethods();

        $argumentNodes = $methodNodes['methodName']->getArguments();
        $argumentNode = $argumentNodes[0];

        $argumentNode->getName()->shouldReturn('__dot_dot_dot__');
    }

    /**
     * @param ReflectionClass  $class
     * @param ReflectionMethod $method
     */
    function it_reflects_protected_abstract_methods($class, $method)
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array($method));
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array());

        $method->isProtected()->willReturn(true);
        $method->isStatic()->willReturn(false);
        $method->getParameters()->willReturn(array());
        $method->getName()->willReturn('innerDetail');
        $method->returnsReference()->willReturn(false);


        $classNode   = $this->reflect($class, array());
        $classNode->shouldBeAnInstanceOf('Prophecy\Doubler\Generator\Node\ClassNode');
        $classNode->getParentClass()->shouldReturn('Custom\ClassName');

        $methodNodes = $classNode->getMethods();
        $methodNodes->shouldHaveCount(1);

        $methodNodes['innerDetail']->getVisibility()->shouldReturn('protected');
    }

    /**
     * @param ReflectionClass  $class
     * @param ReflectionMethod $method
     */
    function it_reflects_public_static_methods($class, $method)
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array($method));
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array());

        $method->isProtected()->willReturn(true);
        $method->isStatic()->willReturn(true);
        $method->getParameters()->willReturn(array());
        $method->getName()->willReturn('innerDetail');
        $method->returnsReference()->willReturn(false);

        $classNode   = $this->reflect($class, array());
        $classNode->shouldBeAnInstanceOf('Prophecy\Doubler\Generator\Node\ClassNode');
        $classNode->getParentClass()->shouldReturn('Custom\ClassName');

        $methodNodes = $classNode->getMethods();
        $methodNodes->shouldHaveCount(1);

        $methodNodes['innerDetail']->getVisibility()->shouldReturn('protected');
        $methodNodes['innerDetail']->isStatic()->shouldReturn(true);
    }

    /**
     * @param ReflectionClass     $class
     * @param ReflectionMethod    $method
     * @param ReflectionParameter $param1
     * @param ReflectionParameter $param2
     * @param ReflectionClass     $typeHint
     * @param ReflectionParameter $param3
     */
    function it_properly_reads_methods_arguments_with_types(
        $class, $method, $param1, $param2, $typeHint, $param3
    )
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method));

        $method->getName()->willReturn('methodWithArgs');
        $method->isFinal()->willReturn(false);
        $method->isProtected()->willReturn(true);
        $method->isStatic()->willReturn(false);
        $method->returnsReference()->willReturn(false);
        $method->getParameters()->willReturn(array($param1, $param2, $param3));

        $param1->getName()->willReturn('arg_1');
        $param1->isArray()->willReturn(true);
        $param1->getClass()->willReturn(null);
        $param1->isDefaultValueAvailable()->willReturn(true);
        $param1->isPassedByReference()->willReturn(false);
        $param1->allowsNull()->willReturn(false);
        $param1->getDefaultValue()->willReturn(array());

        $param2->getName()->willReturn('arg2');
        $param2->isArray()->willReturn(false);
        $param2->getClass()->willReturn($typeHint);
        $param2->isDefaultValueAvailable()->willReturn(false);
        $param2->isOptional()->willReturn(false);
        $param2->isPassedByReference()->willReturn(false);
        $param2->allowsNull()->willReturn(false);
        $typeHint->getName()->willReturn('ArrayAccess');

        $param3->getName()->willReturn('arg_3');
        $param3->isArray()->willReturn(false);
        if (version_compare(PHP_VERSION, '5.4', '>=')) {
            $param3->isCallable()->willReturn(true);
        }
        $param3->getClass()->willReturn(null);
        $param3->isOptional()->willReturn(false);
        $param3->isDefaultValueAvailable()->willReturn(false);
        $param3->isPassedByReference()->willReturn(false);
        $param3->allowsNull()->willReturn(true);

        $classNode   = $this->reflect($class, array());
        $methodNodes = $classNode->getMethods();
        $argNodes    = $methodNodes['methodWithArgs']->getArguments();

        $argNodes[0]->getName()->shouldReturn('arg_1');
        $argNodes[0]->getTypeHint()->shouldReturn('array');
        $argNodes[0]->isOptional()->shouldReturn(true);
        $argNodes[0]->getDefault()->shouldReturn(array());

        $argNodes[1]->getName()->shouldReturn('arg2');
        $argNodes[1]->getTypeHint()->shouldReturn('ArrayAccess');
        $argNodes[1]->isOptional()->shouldReturn(false);

        $argNodes[2]->getName()->shouldReturn('arg_3');
        if (version_compare(PHP_VERSION, '5.4', '>=')) {
            $argNodes[2]->getTypeHint()->shouldReturn('callable');
            $argNodes[2]->isOptional()->shouldReturn(true);
            $argNodes[2]->getDefault()->shouldReturn(null);
        } else {
            $argNodes[2]->isOptional()->shouldReturn(false);
        }
    }

    /**
     * @param ReflectionClass     $class
     * @param ReflectionMethod    $method
     * @param ReflectionParameter $param1
     */
    function it_marks_required_args_without_types_as_not_optional(
        $class, $method, $param1
    )
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method));

        $method->getName()->willReturn('methodWithArgs');
        $method->isFinal()->willReturn(false);
        $method->isProtected()->willReturn(false);
        $method->isStatic()->willReturn(false);
        $method->returnsReference()->willReturn(false);
        $method->getParameters()->willReturn(array($param1));

        $param1->getName()->willReturn('arg_1');
        $param1->isArray()->willReturn(false);
        if (version_compare(PHP_VERSION, '5.4', '>=')) {
            $param1->isCallable()->willReturn(false);
        }
        $param1->getClass()->willReturn(null);
        $param1->isDefaultValueAvailable()->willReturn(false);
        $param1->isOptional()->willReturn(false);
        $param1->isPassedByReference()->willReturn(false);
        $param1->allowsNull()->willReturn(true);
        if (defined('HHVM_VERSION')) {
            $param1->getTypehintText()->willReturn(null);
        }

        $classNode   = $this->reflect($class, array());
        $methodNodes = $classNode->getMethods();
        $argNodes    = $methodNodes['methodWithArgs']->getArguments();

        $argNodes[0]->isOptional()->shouldReturn(false);
    }

    /**
     * @param ReflectionClass     $class
     * @param ReflectionMethod    $method
     * @param ReflectionParameter $param1
     * @param ReflectionParameter $param2
     * @param ReflectionClass     $typeHint
     */
    function it_marks_passed_by_reference_args_as_passed_by_reference(
        $class, $method, $param1, $param2, $typeHint
    )
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method));

        $method->getName()->willReturn('methodWithArgs');
        $method->isFinal()->willReturn(false);
        $method->isProtected()->willReturn(false);
        $method->isStatic()->willReturn(false);
        $method->returnsReference()->willReturn(false);
        $method->getParameters()->willReturn(array($param1, $param2));

        $param1->getName()->willReturn('arg_1');
        $param1->isArray()->willReturn(false);
        if (version_compare(PHP_VERSION, '5.4', '>=')) {
            $param1->isCallable()->willReturn(false);
        }
        $param1->getClass()->willReturn(null);
        $param1->isDefaultValueAvailable()->willReturn(false);
        $param1->isOptional()->willReturn(true);
        $param1->isPassedByReference()->willReturn(true);
        $param1->allowsNull()->willReturn(false);
        if (defined('HHVM_VERSION')) {
            $param1->getTypehintText()->willReturn(null);
        }

        $param2->getName()->willReturn('arg2');
        $param2->isArray()->willReturn(false);
        $param2->getClass()->willReturn($typeHint);
        $param2->isDefaultValueAvailable()->willReturn(false);
        $param2->isOptional()->willReturn(false);
        $param2->isPassedByReference()->willReturn(false);
        $param2->allowsNull()->willReturn(false);
        $typeHint->getName()->willReturn('ArrayAccess');

        $classNode   = $this->reflect($class, array());
        $methodNodes = $classNode->getMethods();
        $argNodes    = $methodNodes['methodWithArgs']->getArguments();

        $argNodes[0]->isPassedByReference()->shouldReturn(true);
        $argNodes[1]->isPassedByReference()->shouldReturn(false);
    }

    /**
     * @param ReflectionClass $class
     */
    function it_throws_an_exception_if_class_is_final($class)
    {
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(true);
        $class->getName()->willReturn('Custom\ClassName');

        $this->shouldThrow('Prophecy\Exception\Doubler\ClassMirrorException')
             ->duringReflect($class, array());
    }

    /**
     * @param ReflectionClass  $class
     * @param ReflectionMethod $method
     */
    function it_ignores_final_methods($class, $method)
    {
        $class->getName()->willReturn('Custom\ClassName');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method));

        $method->isFinal()->willReturn(true);
        $method->getName()->willReturn('finalImplementation');

        $classNode = $this->reflect($class, array());
        $classNode->getMethods()->shouldHaveCount(0);
    }

    /**
     * @param ReflectionClass $interface
     */
    function it_throws_an_exception_if_interface_provided_instead_of_class($interface)
    {
        $interface->isInterface()->willReturn(true);
        $interface->getName()->willReturn('Custom\ClassName');

        $this->shouldThrow('Prophecy\Exception\InvalidArgumentException')
             ->duringReflect($interface, array());
    }

    /**
     * @param ReflectionClass  $interface1
     * @param ReflectionClass  $interface2
     * @param ReflectionMethod $method1
     * @param ReflectionMethod $method2
     * @param ReflectionMethod $method3
     */
    function it_reflects_all_interfaces_methods(
        $interface1, $interface2, $method1, $method2, $method3
    )
    {
        $interface1->getName()->willReturn('MyInterface1');
        $interface2->getName()->willReturn('MyInterface2');

        $interface1->isInterface()->willReturn(true);
        $interface2->isInterface()->willReturn(true);

        $interface1->getMethods()->willReturn(array($method1));
        $interface2->getMethods()->willReturn(array($method2, $method3));

        $method1->getName()->willReturn('getName');
        $method2->getName()->willReturn('isPublic');
        $method3->getName()->willReturn('isAbstract');

        $method1->isProtected()->willReturn(false);
        $method2->isProtected()->willReturn(false);
        $method3->isProtected()->willReturn(false);

        $method1->returnsReference()->willReturn(false);
        $method2->returnsReference()->willReturn(false);
        $method3->returnsReference()->willReturn(false);

        $method1->isStatic()->willReturn(false);
        $method2->isStatic()->willReturn(false);
        $method3->isStatic()->willReturn(false);

        $method1->getParameters()->willReturn(array());
        $method2->getParameters()->willReturn(array());
        $method3->getParameters()->willReturn(array());

        $classNode = $this->reflect(null, array($interface1, $interface2));

        $classNode->shouldBeAnInstanceOf('Prophecy\Doubler\Generator\Node\ClassNode');
        $classNode->getParentClass()->shouldReturn('stdClass');
        $classNode->getInterfaces()->shouldReturn(array(
            'Prophecy\Doubler\Generator\ReflectionInterface', 'MyInterface2', 'MyInterface1',
        ));

        $methodNodes = $classNode->getMethods();
        $methodNodes->shouldHaveCount(3);

        $classNode->hasMethod('getName')->shouldReturn(true);
        $classNode->hasMethod('isPublic')->shouldReturn(true);
        $classNode->hasMethod('isAbstract')->shouldReturn(true);
    }

    /**
     * @param ReflectionClass  $class
     * @param ReflectionMethod $method1
     * @param ReflectionMethod $method2
     * @param ReflectionMethod $method3
     */
    function it_ignores_virtually_private_methods($class, $method1, $method2, $method3)
    {
        $class->getName()->willReturn('SomeClass');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method1, $method2, $method3));

        $method1->getName()->willReturn('_getName');
        $method2->getName()->willReturn('__toString');
        $method3->getName()->willReturn('isAbstract');

        $method1->isFinal()->willReturn(false);
        $method2->isFinal()->willReturn(false);
        $method3->isFinal()->willReturn(false);

        $method1->isProtected()->willReturn(false);
        $method2->isProtected()->willReturn(false);
        $method3->isProtected()->willReturn(false);

        $method1->isStatic()->willReturn(false);
        $method2->isStatic()->willReturn(false);
        $method3->isStatic()->willReturn(false);

        $method1->returnsReference()->willReturn(false);
        $method2->returnsReference()->willReturn(false);
        $method3->returnsReference()->willReturn(false);

        $method1->getParameters()->willReturn(array());
        $method2->getParameters()->willReturn(array());
        $method3->getParameters()->willReturn(array());

        $classNode = $this->reflect($class, array());
        $methodNodes = $classNode->getMethods();
        $methodNodes->shouldHaveCount(2);

        $classNode->hasMethod('isAbstract')->shouldReturn(true);
    }

    /**
     * @param ReflectionClass  $class
     * @param ReflectionMethod $method
     */
    function it_does_not_throw_exception_for_virtually_private_finals($class, $method)
    {
        $class->getName()->willReturn('SomeClass');
        $class->isInterface()->willReturn(false);
        $class->isFinal()->willReturn(false);
        $class->getMethods(ReflectionMethod::IS_ABSTRACT)->willReturn(array());
        $class->getMethods(ReflectionMethod::IS_PUBLIC)->willReturn(array($method));

        $method->getName()->willReturn('__toString');
        $method->isFinal()->willReturn(true);

        $this->shouldNotThrow()->duringReflect($class, array());
    }

    /**
     * @param ReflectionClass $class
     */
    function it_throws_an_exception_if_class_provided_in_interfaces_list($class)
    {
        $class->getName()->willReturn('MyClass');
        $class->isInterface()->willReturn(false);

        $this->shouldThrow('InvalidArgumentException')
             ->duringReflect(null, array($class));
    }

    function it_throws_an_exception_if_not_reflection_provided_as_interface()
    {
        $this->shouldThrow('InvalidArgumentException')
             ->duringReflect(null, array(null));
    }

    function it_doesnt_fail_to_typehint_nonexistent_FQCN()
    {
        $classNode = $this->reflect(new ReflectionClass('spec\Prophecy\Doubler\Generator\OptionalDepsClass'), array());
        $method = $classNode->getMethod('iHaveAStrangeTypeHintedArg');
        $arguments = $method->getArguments();
        $arguments[0]->getTypeHint()->shouldBe('I\Simply\Am\Nonexistent');
    }

    function it_doesnt_fail_to_typehint_nonexistent_RQCN()
    {
        $classNode = $this->reflect(new ReflectionClass('spec\Prophecy\Doubler\Generator\OptionalDepsClass'), array());
        $method = $classNode->getMethod('iHaveAnEvenStrangerTypeHintedArg');
        $arguments = $method->getArguments();
        $arguments[0]->getTypeHint()->shouldBe('I\Simply\Am\Not');
    }

    function it_doesnt_use_scalar_typehints()
    {
        $classNode = $this->reflect(new ReflectionClass('ReflectionMethod'), array());
        $method = $classNode->getMethod('export');
        $arguments = $method->getArguments();
        $arguments[0]->getTypeHint()->shouldReturn(null);
        $arguments[1]->getTypeHint()->shouldReturn(null);
        $arguments[2]->getTypeHint()->shouldReturn(null);
    }
}

class OptionalDepsClass
{
    public function iHaveAStrangeTypeHintedArg(\I\Simply\Am\Nonexistent $class)
    {
    }

    public function iHaveAnEvenStrangerTypeHintedArg(Simply\Am\Not $class)
    {
    }
}
