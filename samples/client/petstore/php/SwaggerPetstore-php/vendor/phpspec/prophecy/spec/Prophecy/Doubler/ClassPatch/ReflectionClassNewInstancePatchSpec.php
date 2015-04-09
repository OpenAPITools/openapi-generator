<?php

namespace spec\Prophecy\Doubler\ClassPatch;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class ReflectionClassNewInstancePatchSpec extends ObjectBehavior
{
    function it_is_a_patch()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Doubler\ClassPatch\ClassPatchInterface');
    }

    function its_priority_is_50()
    {
        $this->getPriority()->shouldReturn(50);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $reflectionClassNode
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $anotherClassNode
     */
    function it_supports_ReflectionClass_only($reflectionClassNode, $anotherClassNode)
    {
        $reflectionClassNode->getParentClass()->willReturn('ReflectionClass');
        $anotherClassNode->getParentClass()->willReturn('stdClass');

        $this->supports($reflectionClassNode)->shouldReturn(true);
        $this->supports($anotherClassNode)->shouldReturn(false);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode    $class
     * @param \Prophecy\Doubler\Generator\Node\MethodNode   $method
     * @param \Prophecy\Doubler\Generator\Node\ArgumentNode $arg1
     * @param \Prophecy\Doubler\Generator\Node\ArgumentNode $arg2
     */
    function it_makes_all_newInstance_arguments_optional($class, $method, $arg1, $arg2)
    {
        $class->getMethod('newInstance')->willReturn($method);
        $method->getArguments()->willReturn(array($arg1));
        $arg1->setDefault(null)->shouldBeCalled();

        $this->apply($class);
    }
}
