<?php
namespace Contrib\Component\Log;

/**
 * @covers Contrib\Component\Log\ConsoleLogger
 *
 * @author Kitamura Satoshi <with.no.parachute@gmail.com>
 */
class ConsoleLoggerTest extends \PHPUnit_Framework_TestCase
{
    protected function createAdapterMockWith($message)
    {
        $mock = $this->getMockBuilder('Symfony\Component\Console\Output\StreamOutput')
        ->disableOriginalConstructor()
        ->setMethods(array('writeln'))
        ->getMock();

        $mock
        ->expects($this->once())
        ->method('writeln')
        ->with($this->equalTo($message));

        return $mock;
    }

    /**
     * @test
     */
    public function shouldWritelnToOutput()
    {
        $message = 'log test message';
        $output = $this->createAdapterMockWith($message);

        $object = new ConsoleLogger($output);

        $object->log('info', $message);
    }
}
