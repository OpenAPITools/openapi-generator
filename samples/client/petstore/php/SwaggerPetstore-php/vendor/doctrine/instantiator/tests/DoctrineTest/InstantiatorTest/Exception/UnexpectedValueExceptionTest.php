<?php
/*
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This software consists of voluntary contributions made by many individuals
 * and is licensed under the MIT license. For more information, see
 * <http://www.doctrine-project.org>.
 */

namespace DoctrineTest\InstantiatorTest\Exception;

use Doctrine\Instantiator\Exception\UnexpectedValueException;
use Exception;
use PHPUnit_Framework_TestCase;
use ReflectionClass;

/**
 * Tests for {@see \Doctrine\Instantiator\Exception\UnexpectedValueException}
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 *
 * @covers \Doctrine\Instantiator\Exception\UnexpectedValueException
 */
class UnexpectedValueExceptionTest extends PHPUnit_Framework_TestCase
{
    public function testFromSerializationTriggeredException()
    {
        $reflectionClass = new ReflectionClass($this);
        $previous        = new Exception();
        $exception       = UnexpectedValueException::fromSerializationTriggeredException($reflectionClass, $previous);

        $this->assertInstanceOf('Doctrine\\Instantiator\\Exception\\UnexpectedValueException', $exception);
        $this->assertSame($previous, $exception->getPrevious());
        $this->assertSame(
            'An exception was raised while trying to instantiate an instance of "'
            . __CLASS__  . '" via un-serialization',
            $exception->getMessage()
        );
    }

    public function testFromUncleanUnSerialization()
    {
        $reflection = new ReflectionClass('DoctrineTest\\InstantiatorTestAsset\\AbstractClassAsset');
        $exception  = UnexpectedValueException::fromUncleanUnSerialization($reflection, 'foo', 123, 'bar', 456);

        $this->assertInstanceOf('Doctrine\\Instantiator\\Exception\\UnexpectedValueException', $exception);
        $this->assertSame(
            'Could not produce an instance of "DoctrineTest\\InstantiatorTestAsset\\AbstractClassAsset" '
            . 'via un-serialization, since an error was triggered in file "bar" at line "456"',
            $exception->getMessage()
        );

        $previous = $exception->getPrevious();

        $this->assertInstanceOf('Exception', $previous);
        $this->assertSame('foo', $previous->getMessage());
        $this->assertSame(123, $previous->getCode());
    }
}
