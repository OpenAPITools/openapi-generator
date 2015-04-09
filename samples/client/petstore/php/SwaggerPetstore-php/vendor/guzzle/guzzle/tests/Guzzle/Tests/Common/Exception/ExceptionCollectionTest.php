<?php

namespace Guzzle\Tests\Common\Exception;

use Guzzle\Common\Exception\ExceptionCollection;

class ExceptionCollectionTest extends \Guzzle\Tests\GuzzleTestCase
{
    private function getExceptions()
    {
        return array(
            new \Exception('Test'),
            new \Exception('Testing')
        );
    }

    public function testAggregatesExceptions()
    {
        $e = new ExceptionCollection();
        $exceptions = $this->getExceptions();
        $e->add($exceptions[0]);
        $e->add($exceptions[1]);
        $this->assertContains("(Exception) ./tests/Guzzle/Tests/Common/Exception/ExceptionCollectionTest.php line ", $e->getMessage());
        $this->assertContains("    Test\n\n    #0 ./", $e->getMessage());
        $this->assertSame($exceptions[0], $e->getFirst());
    }

    public function testCanSetExceptions()
    {
        $ex = new \Exception('foo');
        $e = new ExceptionCollection();
        $e->setExceptions(array($ex));
        $this->assertSame($ex, $e->getFirst());
    }

    public function testActsAsArray()
    {
        $e = new ExceptionCollection();
        $exceptions = $this->getExceptions();
        $e->add($exceptions[0]);
        $e->add($exceptions[1]);
        $this->assertEquals(2, count($e));
        $this->assertEquals($exceptions, $e->getIterator()->getArrayCopy());
    }

    public function testCanAddSelf()
    {
        $e1 = new ExceptionCollection();
        $e1->add(new \Exception("Test"));
        $e2 = new ExceptionCollection('Meta description!');
        $e2->add(new \Exception("Test 2"));
        $e3 = new ExceptionCollection();
        $e3->add(new \Exception('Baz'));
        $e2->add($e3);
        $e1->add($e2);
        $message = $e1->getMessage();
        $this->assertContains("(Exception) ./tests/Guzzle/Tests/Common/Exception/ExceptionCollectionTest.php line ", $message);
        $this->assertContains("\n    Test\n\n    #0 ", $message);
        $this->assertContains("\n\n(Guzzle\\Common\\Exception\\ExceptionCollection) ./tests/Guzzle/Tests/Common/Exception/ExceptionCollectionTest.php line ", $message);
        $this->assertContains("\n\n    Meta description!\n\n", $message);
        $this->assertContains("    (Exception) ./tests/Guzzle/Tests/Common/Exception/ExceptionCollectionTest.php line ", $message);
        $this->assertContains("\n        Test 2\n\n        #0 ", $message);
        $this->assertContains("        (Exception) ./tests/Guzzle/Tests/Common/Exception/ExceptionCollectionTest.php line ", $message);
        $this->assertContains("            Baz\n\n            #0", $message);
    }
}
