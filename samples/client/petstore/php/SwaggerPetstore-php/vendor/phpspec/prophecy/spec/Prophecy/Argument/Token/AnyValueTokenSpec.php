<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class AnyValueTokenSpec extends ObjectBehavior
{
    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function its_string_representation_is_star()
    {
        $this->__toString()->shouldReturn('*');
    }

    function it_scores_any_argument_as_3()
    {
        $this->scoreArgument(42)->shouldReturn(3);
    }
}
