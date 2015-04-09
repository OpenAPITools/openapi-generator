<?php

namespace spec\Prophecy\Doubler\Generator\Node;

use PhpSpec\ObjectBehavior;

class ArgumentNodeSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('name');
    }

    function it_is_not_be_passed_by_reference_by_default()
    {
        $this->shouldNotBePassedByReference();
    }

    function it_is_passed_by_reference_if_marked()
    {
        $this->setAsPassedByReference();
        $this->shouldBePassedByReference();
    }

    function it_has_name_with_which_it_was_been_constructed()
    {
        $this->getName()->shouldReturn('name');
    }

    function it_has_no_typehint_by_default()
    {
        $this->getTypeHint()->shouldReturn(null);
    }

    function its_typeHint_is_mutable()
    {
        $this->setTypeHint('array');
        $this->getTypeHint()->shouldReturn('array');
    }

    function it_does_not_have_default_value_by_default()
    {
        $this->getDefault()->shouldReturn(null);
    }

    function it_is_not_optional_by_default()
    {
        $this->isOptional()->shouldReturn(false);
    }

    function its_default_is_mutable()
    {
        $this->setDefault(array());
        $this->getDefault()->shouldReturn(array());
    }

    function it_is_marked_as_optional_when_default_is_set()
    {
        $this->setDefault(null);
        $this->isOptional()->shouldReturn(true);
    }
}
