<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class ArrayCountTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(2);
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_scores_6_if_argument_array_has_proper_count()
    {
        $this->scoreArgument(array(1,2))->shouldReturn(6);
    }

    /**
     * @param \Countable $countable
     */
    function it_scores_6_if_argument_countable_object_has_proper_count($countable)
    {
        $countable->count()->willReturn(2);
        $this->scoreArgument($countable)->shouldReturn(6);
    }

    function it_does_not_score_if_argument_is_neither_array_nor_countable_object()
    {
        $this->scoreArgument('string')->shouldBe(false);
        $this->scoreArgument(5)->shouldBe(false);
        $this->scoreArgument(new \stdClass)->shouldBe(false);
    }

    function it_does_not_score_if_argument_array_has_wrong_count()
    {
        $this->scoreArgument(array(1))->shouldReturn(false);
    }

    /**
     * @param \Countable $countable
     */
    function it_does_not_score_if_argument_countable_object_has_wrong_count($countable)
    {
        $countable->count()->willReturn(3);
        $this->scoreArgument($countable)->shouldReturn(false);
    }

    function it_has_simple_string_representation()
    {
        $this->__toString()->shouldBe('count(2)');
    }

}
