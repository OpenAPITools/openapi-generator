<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class TypeTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('integer');
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_scores_5_if_argument_matches_simple_type()
    {
        $this->beConstructedWith('integer');

        $this->scoreArgument(42)->shouldReturn(5);
    }

    function it_does_not_scores_if_argument_does_not_match_simple_type()
    {
        $this->beConstructedWith('integer');

        $this->scoreArgument(42.0)->shouldReturn(false);
    }

    /**
     * @param \ReflectionObject $object
     */
    function it_scores_5_if_argument_is_an_instance_of_specified_class($object)
    {
        $this->beConstructedWith('ReflectionClass');

        $this->scoreArgument($object)->shouldReturn(5);
    }

    function it_has_simple_string_representation()
    {
        $this->__toString()->shouldReturn('type(integer)');
    }

    function it_scores_5_if_argument_is_an_instance_of_specified_interface(\Prophecy\Argument\Token\TokenInterface $interface)
    {
        $this->beConstructedWith('Prophecy\Argument\Token\TokenInterface');

        $this->scoreArgument($interface)->shouldReturn(5);
    }
}
