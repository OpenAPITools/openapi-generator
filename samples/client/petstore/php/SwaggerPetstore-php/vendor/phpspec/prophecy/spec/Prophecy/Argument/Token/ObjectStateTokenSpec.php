<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class ObjectStateTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith('getName', 'stdClass');
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    /**
     * @param \ReflectionClass $reflection
     */
    function it_scores_8_if_argument_object_has_specific_method_state($reflection)
    {
        $reflection->getName()->willReturn('stdClass');

        $this->scoreArgument($reflection)->shouldReturn(8);
    }

    /**
     * @param \stdClass $class
     */
    function it_scores_8_if_argument_object_has_specific_property_state($class)
    {
        $class->getName = 'stdClass';

        $this->scoreArgument($class)->shouldReturn(8);
    }

    function it_does_not_score_if_argument_method_state_does_not_match()
    {
        $value = new ObjectStateTokenFixtureB('ABC');
        $value2 = new ObjectStateTokenFixtureB('CBA');

        $this->beConstructedWith('getSelf', $value);
        $this->scoreArgument($value2)->shouldReturn(false);
    }

    /**
     * @param \stdClass $class
     */
    function it_does_not_score_if_argument_property_state_does_not_match($class)
    {
        $class->getName = 'SplFileInfo';

        $this->scoreArgument($class)->shouldReturn(false);
    }

    /**
     * @param \spec\Prophecy\Argument\Token\ObjectStateTokenFixtureA $class
     */
    function it_does_not_score_if_argument_object_does_not_have_method_or_property($class)
    {
        $this->scoreArgument($class)->shouldReturn(false);
    }

    function it_does_not_score_if_argument_is_not_object()
    {
        $this->scoreArgument(42)->shouldReturn(false);
    }

    function it_has_simple_string_representation()
    {
        $this->__toString()->shouldReturn('state(getName(), "stdClass")');
    }
}

class ObjectStateTokenFixtureA
{
    public $errors;
}

class ObjectStateTokenFixtureB extends ObjectStateTokenFixtureA
{
    public $errors;
    public $value = null;

    public function __construct($value)
    {
        $this->value = $value;
    }

    public function getSelf()
    {
        return $this;
    }
}
