<?php

namespace spec\Prophecy\Prophecy;

use PhpSpec\ObjectBehavior;
use Prophecy\Argument;

class ObjectProphecySpec extends ObjectBehavior
{
    /**
     * @param \Prophecy\Doubler\LazyDouble                $lazyDouble
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $double
     */
    function let($lazyDouble, $double)
    {
        $this->beConstructedWith($lazyDouble);

        $lazyDouble->getInstance()->willReturn($double);
    }

    function it_implements_ProphecyInterface()
    {
        $this->shouldBeAnInstanceOf('Prophecy\Prophecy\ProphecyInterface');
    }

    function it_sets_parentClass_during_willExtend_call($lazyDouble)
    {
        $lazyDouble->setParentClass('123')->shouldBeCalled();

        $this->willExtend('123');
    }

    function it_adds_interface_during_willImplement_call($lazyDouble)
    {
        $lazyDouble->addInterface('222')->shouldBeCalled();

        $this->willImplement('222');
    }

    function it_sets_constructor_arguments_during_willBeConstructedWith_call($lazyDouble)
    {
        $lazyDouble->setArguments(array(1, 2, 5))->shouldBeCalled();

        $this->willBeConstructedWith(array(1, 2, 5));
    }

    function it_does_not_have_method_prophecies_by_default()
    {
        $this->getMethodProphecies()->shouldHaveCount(0);
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy    $method1
     * @param \Prophecy\Prophecy\MethodProphecy    $method2
     * @param \Prophecy\Argument\ArgumentsWildcard $arguments
     */
    function it_should_get_method_prophecies_by_method_name($method1, $method2, $arguments)
    {
        $method1->getMethodName()->willReturn('getName');
        $method1->getArgumentsWildcard()->willReturn($arguments);
        $method2->getMethodName()->willReturn('setName');
        $method2->getArgumentsWildcard()->willReturn($arguments);

        $this->addMethodProphecy($method1);
        $this->addMethodProphecy($method2);

        $methods = $this->getMethodProphecies('setName');
        $methods->shouldHaveCount(1);
        $methods[0]->getMethodName()->shouldReturn('setName');
    }

    function it_should_return_empty_array_if_no_method_prophecies_found()
    {
        $methods = $this->getMethodProphecies('setName');
        $methods->shouldHaveCount(0);
    }

    /**
     * @param \Prophecy\Call\CallCenter $callCenter
     */
    function it_should_proxy_makeProphecyMethodCall_to_CallCenter($lazyDouble, $callCenter)
    {
        $this->beConstructedWith($lazyDouble, $callCenter);

        $callCenter->makeCall($this->getWrappedObject(), 'setName', array('everzet'))->willReturn(42);

        $this->makeProphecyMethodCall('setName', array('everzet'))->shouldReturn(42);
    }

    /**
     * @param \Prophecy\Call\CallCenter            $callCenter
     * @param \Prophecy\Prophecy\RevealerInterface $revealer
     */
    function it_should_reveal_arguments_and_return_values_from_callCenter(
        $lazyDouble, $callCenter, $revealer
    )
    {
        $this->beConstructedWith($lazyDouble, $callCenter, $revealer);

        $revealer->reveal(array('question'))->willReturn(array('life'));
        $revealer->reveal('answer')->willReturn(42);

        $callCenter->makeCall($this->getWrappedObject(), 'setName', array('life'))->willReturn('answer');

        $this->makeProphecyMethodCall('setName', array('question'))->shouldReturn(42);
    }

    /**
     * @param \Prophecy\Call\CallCenter            $callCenter
     * @param \Prophecy\Argument\ArgumentsWildcard $wildcard
     * @param \Prophecy\Call\Call                  $call
     */
    function it_should_proxy_getProphecyMethodCalls_to_CallCenter(
        $lazyDouble, $callCenter, $wildcard, $call
    )
    {
        $this->beConstructedWith($lazyDouble, $callCenter);

        $callCenter->findCalls('setName', $wildcard)->willReturn(array($call));

        $this->findProphecyMethodCalls('setName', $wildcard)->shouldReturn(array($call));
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard
     */
    function its_addMethodProphecy_adds_method_prophecy(
        $methodProphecy, $argumentsWildcard
    )
    {
        $methodProphecy->getArgumentsWildcard()->willReturn($argumentsWildcard);
        $methodProphecy->getMethodName()->willReturn('getUsername');

        $this->addMethodProphecy($methodProphecy);

        $this->getMethodProphecies()->shouldReturn(array(
            'getUsername' => array($methodProphecy)
        ));
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy1
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy2
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard1
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard2
     */
    function its_addMethodProphecy_handles_prophecies_with_different_arguments(
        $methodProphecy1, $methodProphecy2, $argumentsWildcard1, $argumentsWildcard2
    )
    {
        $methodProphecy1->getArgumentsWildcard()->willReturn($argumentsWildcard1);
        $methodProphecy1->getMethodName()->willReturn('getUsername');

        $methodProphecy2->getArgumentsWildcard()->willReturn($argumentsWildcard2);
        $methodProphecy2->getMethodName()->willReturn('getUsername');

        $this->addMethodProphecy($methodProphecy1);
        $this->addMethodProphecy($methodProphecy2);

        $this->getMethodProphecies()->shouldReturn(array(
            'getUsername' => array(
                $methodProphecy1,
                $methodProphecy2,
            )
        ));
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy1
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy2
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard1
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard2
     */
    function its_addMethodProphecy_handles_prophecies_for_different_methods(
        $methodProphecy1, $methodProphecy2, $argumentsWildcard1, $argumentsWildcard2
    )
    {
        $methodProphecy1->getArgumentsWildcard()->willReturn($argumentsWildcard1);
        $methodProphecy1->getMethodName()->willReturn('getUsername');

        $methodProphecy2->getArgumentsWildcard()->willReturn($argumentsWildcard2);
        $methodProphecy2->getMethodName()->willReturn('isUsername');

        $this->addMethodProphecy($methodProphecy1);
        $this->addMethodProphecy($methodProphecy2);

        $this->getMethodProphecies()->shouldReturn(array(
            'getUsername' => array(
                $methodProphecy1
            ),
            'isUsername' => array(
                $methodProphecy2
            )
        ));
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy $methodProphecy
     */
    function its_addMethodProphecy_throws_exception_when_method_has_no_ArgumentsWildcard(
        $methodProphecy
    )
    {
        $methodProphecy->getArgumentsWildcard()->willReturn(null);
        $methodProphecy->getObjectProphecy()->willReturn($this);
        $methodProphecy->getMethodName()->willReturn('getTitle');

        $this->shouldThrow('Prophecy\Exception\Prophecy\MethodProphecyException')->duringAddMethodProphecy(
            $methodProphecy
        );
    }

    function it_returns_null_after_checkPredictions_call_if_there_is_no_method_prophecies()
    {
        $this->checkProphecyMethodsPredictions()->shouldReturn(null);
    }

    /**
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy1
     * @param \Prophecy\Prophecy\MethodProphecy    $methodProphecy2
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard1
     * @param \Prophecy\Argument\ArgumentsWildcard $argumentsWildcard2
     */
    function it_throws_AggregateException_during_checkPredictions_if_predictions_fail(
        $methodProphecy1, $methodProphecy2, $argumentsWildcard1, $argumentsWildcard2
    )
    {
        $methodProphecy1->getMethodName()->willReturn('getName');
        $methodProphecy1->getArgumentsWildcard()->willReturn($argumentsWildcard1);
        $methodProphecy1->checkPrediction()
            ->willThrow('Prophecy\Exception\Prediction\AggregateException');

        $methodProphecy2->getMethodName()->willReturn('setName');
        $methodProphecy2->getArgumentsWildcard()->willReturn($argumentsWildcard2);
        $methodProphecy2->checkPrediction()
            ->willThrow('Prophecy\Exception\Prediction\AggregateException');

        $this->addMethodProphecy($methodProphecy1);
        $this->addMethodProphecy($methodProphecy2);

        $this->shouldThrow('Prophecy\Exception\Prediction\AggregateException')
            ->duringCheckProphecyMethodsPredictions();
    }

    /**
     * @param \Prophecy\Doubler\Doubler                   $doubler
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $reflection
     */
    function it_returns_new_MethodProphecy_instance_for_arbitrary_call($doubler, $reflection)
    {
        $doubler->double(Argument::any())->willReturn($reflection);

        $return = $this->getProphecy();
        $return->shouldBeAnInstanceOf('Prophecy\Prophecy\MethodProphecy');
        $return->getMethodName()->shouldReturn('getProphecy');
    }

    /**
     * @param \Prophecy\Doubler\Doubler                   $doubler
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $reflection
     */
    function it_returns_same_MethodProphecy_for_same_registered_signature($doubler, $reflection)
    {
        $doubler->double(Argument::any())->willReturn($reflection);

        $this->addMethodProphecy($methodProphecy1 = $this->getProphecy(1, 2, 3));
        $methodProphecy2 = $this->getProphecy(1, 2, 3);

        $methodProphecy2->shouldBe($methodProphecy1);
    }

    /**
     * @param \Prophecy\Doubler\Doubler                   $doubler
     * @param \Prophecy\Prophecy\ProphecySubjectInterface $reflection
     */
    function it_returns_new_MethodProphecy_for_different_signatures($doubler, $reflection)
    {
        $doubler->double(Argument::any())->willReturn($reflection);

        $value = new ObjectProphecySpecFixtureB('ABC');
        $value2 = new ObjectProphecySpecFixtureB('CBA');

        $this->addMethodProphecy($methodProphecy1 = $this->getProphecy(1, 2, 3, $value));
        $methodProphecy2 = $this->getProphecy(1, 2, 3, $value2);

        $methodProphecy2->shouldNotBe($methodProphecy1);
    }
}

class ObjectProphecySpecFixtureA
{
	public $errors;
}

class ObjectProphecySpecFixtureB extends ObjectProphecySpecFixtureA
{
    public $errors;
    public $value = null;

    public function __construct($value)
    {
        $this->value = $value;
    }
}
