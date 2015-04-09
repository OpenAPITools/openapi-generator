<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class AnyValuesTokenSpec extends ObjectBehavior
{
    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_last()
    {
        $this->shouldBeLast();
    }

    function its_string_representation_is_star_with_followup()
    {
        $this->__toString()->shouldReturn('* [, ...]');
    }

    function it_scores_any_argument_as_2()
    {
        $this->scoreArgument(42)->shouldReturn(2);
    }
}
