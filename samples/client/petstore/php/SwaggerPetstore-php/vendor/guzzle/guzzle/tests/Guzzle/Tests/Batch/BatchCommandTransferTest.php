<?php

namespace Guzzle\Tests\Batch;

use Guzzle\Batch\BatchCommandTransfer;
use Guzzle\Service\Client;
use Guzzle\Tests\Service\Mock\Command\MockCommand as Mc;

/**
 * @covers Guzzle\Batch\BatchCommandTransfer
 */
class BatchCommandTransferTest extends \Guzzle\Tests\GuzzleTestCase
{
    public function testCreatesBatchesBasedOnClient()
    {
        $client1 = new Client('http://www.example.com');
        $client2 = new Client('http://www.example.com');

        $commands = array(new Mc(), new Mc(), new Mc(), new Mc(), new Mc());

        $queue = new \SplQueue();
        foreach ($commands as $i => $command) {
            if ($i % 2) {
                $command->setClient($client1);
            } else {
                $command->setClient($client2);
            }
            $queue[] = $command;
        }

        $batch = new BatchCommandTransfer(2);
        $this->assertEquals(array(
            array($commands[0], $commands[2]),
            array($commands[4]),
            array($commands[1], $commands[3])
        ), $batch->createBatches($queue));
    }

    /**
     * @expectedException Guzzle\Common\Exception\InvalidArgumentException
     */
    public function testEnsuresAllItemsAreCommands()
    {
        $queue = new \SplQueue();
        $queue[] = 'foo';
        $batch = new BatchCommandTransfer(2);
        $batch->createBatches($queue);
    }

    public function testTransfersBatches()
    {
        $client = $this->getMockBuilder('Guzzle\Service\Client')
            ->setMethods(array('send'))
            ->getMock();
        $client->expects($this->once())
            ->method('send');
        $command = new Mc();
        $command->setClient($client);
        $batch = new BatchCommandTransfer(2);
        $batch->transfer(array($command));
    }

    public function testDoesNotTransfersEmptyBatches()
    {
        $batch = new BatchCommandTransfer(2);
        $batch->transfer(array());
    }

    /**
     * @expectedException Guzzle\Service\Exception\InconsistentClientTransferException
     */
    public function testEnsuresAllCommandsUseTheSameClient()
    {
        $batch = new BatchCommandTransfer(2);
        $client1 = new Client('http://www.example.com');
        $client2 = new Client('http://www.example.com');
        $command1 = new Mc();
        $command1->setClient($client1);
        $command2 = new Mc();
        $command2->setClient($client2);
        $batch->transfer(array($command1, $command2));
    }
}
