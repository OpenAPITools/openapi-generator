<?php

namespace spec\Prophecy\Argument\Token;

use PhpSpec\ObjectBehavior;

class ExactValueTokenSpec extends ObjectBehavior
{
    function let()
    {
        $this->beConstructedWith(42);
    }

    function it_implements_TokenInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Argument\Token\TokenInterface');
    }

    function it_is_not_last()
    {
        $this->shouldNotBeLast();
    }

    function it_holds_value()
    {
        $this->getValue()->shouldReturn(42);
    }

    function it_scores_10_if_value_is_equal_to_argument()
    {
        $this->scoreArgument(42)->shouldReturn(10);
        $this->scoreArgument('42')->shouldReturn(10);
    }

    function it_scores_10_if_value_is_an_object_and_equal_to_argument()
    {
        $value = new \DateTime();
        $value2 = clone $value;

        $this->beConstructedWith($value);
        $this->scoreArgument($value2)->shouldReturn(10);
    }

    function it_does_not_scores_if_value_is_not_equal_to_argument()
    {
        $this->scoreArgument(50)->shouldReturn(false);
        $this->scoreArgument(new \stdClass())->shouldReturn(false);
    }

    function it_does_not_scores_if_value_an_object_and_is_not_equal_to_argument()
    {
        $value = new ExactValueTokenFixtureB('ABC');
        $value2 = new ExactValueTokenFixtureB('CBA');

        $this->beConstructedWith($value);
        $this->scoreArgument($value2)->shouldReturn(false);
    }

    function it_does_not_scores_if_value_type_and_is_not_equal_to_argument()
    {
        $this->beConstructedWith(false);
        $this->scoreArgument(0)->shouldReturn(false);
    }

    function it_generates_proper_string_representation_for_integer()
    {
        $this->beConstructedWith(42);
        $this->__toString()->shouldReturn('exact(42)');
    }

    function it_generates_proper_string_representation_for_string()
    {
        $this->beConstructedWith('some string');
        $this->__toString()->shouldReturn('exact("some string")');
    }

    function it_generates_single_line_representation_for_multiline_string()
    {
        $this->beConstructedWith("some\nstring");
        $this->__toString()->shouldReturn('exact("some\\nstring")');
    }

    function it_generates_proper_string_representation_for_double()
    {
        $this->beConstructedWith(42.3);
        $this->__toString()->shouldReturn('exact(42.3)');
    }

    function it_generates_proper_string_representation_for_boolean_true()
    {
        $this->beConstructedWith(true);
        $this->__toString()->shouldReturn('exact(true)');
    }

    function it_generates_proper_string_representation_for_boolean_false()
    {
        $this->beConstructedWith(false);
        $this->__toString()->shouldReturn('exact(false)');
    }

    function it_generates_proper_string_representation_for_null()
    {
        $this->beConstructedWith(null);
        $this->__toString()->shouldReturn('exact(null)');
    }

    function it_generates_proper_string_representation_for_empty_array()
    {
        $this->beConstructedWith(array());
        $this->__toString()->shouldReturn('exact([])');
    }

    function it_generates_proper_string_representation_for_array()
    {
        $this->beConstructedWith(array('zet', 42));
        $this->__toString()->shouldReturn('exact(["zet", 42])');
    }

    function it_generates_proper_string_representation_for_resource()
    {
        $resource = fopen(__FILE__, 'r');
        $this->beConstructedWith($resource);
        $this->__toString()->shouldReturn('exact(stream:'.$resource.')');
    }

    /**
     * @param \stdClass $object
     */
    function it_generates_proper_string_representation_for_object($object)
    {
        $objHash = sprintf('%s:%s',
            get_class($object->getWrappedObject()),
            spl_object_hash($object->getWrappedObject())
        );

        $this->beConstructedWith($object);
        $this->__toString()->shouldReturn("exact($objHash Object (\n    'objectProphecy' => Prophecy\Prophecy\ObjectProphecy Object (*Prophecy*)\n))");
    }
}

class ExactValueTokenFixtureA
{
    public $errors;
}

class ExactValueTokenFixtureB extends ExactValueTokenFixtureA
{
    public $errors;
    public $value = null;

    public function __construct($value)
    {
        $this->value = $value;
    }
}
