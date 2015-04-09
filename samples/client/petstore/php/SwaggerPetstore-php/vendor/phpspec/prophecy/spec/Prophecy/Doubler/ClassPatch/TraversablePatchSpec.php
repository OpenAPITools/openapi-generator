<?php

namespace spec\Prophecy\Doubler\ClassPatch;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class TraversablePatchSpec extends ObjectBehavior
{
    function it_is_a_patch()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Doubler\ClassPatch\ClassPatchInterface');
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $node
     */
    function it_supports_class_that_implements_only_Traversable($node)
    {
        $node->getInterfaces()->willReturn(array('Traversable'));

        $this->supports($node)->shouldReturn(true);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $node
     */
    function it_does_not_support_class_that_implements_Iterator($node)
    {
        $node->getInterfaces()->willReturn(array('Traversable', 'Iterator'));

        $this->supports($node)->shouldReturn(false);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $node
     */
    function it_does_not_support_class_that_implements_IteratorAggregate($node)
    {
        $node->getInterfaces()->willReturn(array('Traversable', 'IteratorAggregate'));

        $this->supports($node)->shouldReturn(false);
    }

    function it_has_100_priority()
    {
        $this->getPriority()->shouldReturn(100);
    }

    /**
     * @param \Prophecy\Doubler\Generator\Node\ClassNode $node
     */
    function it_forces_node_to_implement_IteratorAggregate($node)
    {
        $node->addInterface('Iterator')->shouldBeCalled();

        $node->addMethod(Argument::type('Prophecy\Doubler\Generator\Node\MethodNode'))->willReturn(null);

        $this->apply($node);
    }
}
