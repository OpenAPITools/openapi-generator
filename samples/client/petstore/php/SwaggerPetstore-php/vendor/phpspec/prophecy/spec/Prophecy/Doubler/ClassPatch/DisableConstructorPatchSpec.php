<?php

namespace spec\Prophecy\Doubler\ClassPatch;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class DisableConstructorPatchSpec extends ObjectBehavior
{
    function it_is_a_patch()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Doubler\ClassPatch\ClassPatchInterface');
    }

    function its_priority_is_100()
    {
        $this->getPriority()->shouldReturn(100);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $node
     */
    function it_supports_anything($node)
    {
        $this->supports($node)->shouldReturn(true);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode    $class
     * @param \Prophecy\Doubler\Generator\Node\MethodNode   $method
     * @param \Prophecy\Doubler\Generator\Node\ArgumentNode $arg1
     * @param \Prophecy\Doubler\Generator\Node\ArgumentNode $arg2
     */
    function it_makes_all_constructor_arguments_optional($class, $method, $arg1, $arg2)
    {
        $class->hasMethod('__construct')->willReturn(true);
        $class->getMethod('__construct')->willReturn($method);
        $method->getArguments()->willReturn(array($arg1, $arg2));

        $arg1->setDefault(null)->shouldBeCalled();
        $arg2->setDefault(null)->shouldBeCalled();

        $method->setCode(Argument::type('string'))->shouldBeCalled();

        $this->apply($class);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $class
     */
    function it_creates_new_constructor_if_object_has_none($class)
    {
        $class->hasMethod('__construct')->willReturn(false);
        $class->addMethod(Argument::type('Prophecy\Doubler\Generator\Node\MethodNode'))
            ->shouldBeCalled();

        $this->apply($class);
    }
}
