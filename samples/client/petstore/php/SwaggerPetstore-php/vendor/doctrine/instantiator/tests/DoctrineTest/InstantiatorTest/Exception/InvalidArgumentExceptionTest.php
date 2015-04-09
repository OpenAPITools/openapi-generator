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

use Doctrine\Instantiator\Exception\InvalidArgumentException;
use PHPUnit_Framework_TestCase;
use ReflectionClass;

/**
 * Tests for {@see \Doctrine\Instantiator\Exception\InvalidArgumentException}
 *
 * @author Marco Pivetta <ocramius@gmail.com>
 *
 * @covers \Doctrine\Instantiator\Exception\InvalidArgumentException
 */
class InvalidArgumentExceptionTest extends PHPUnit_Framework_TestCase
{
    public function testFromNonExistingTypeWithNonExistingClass()
    {
        $className = __CLASS__ . uniqid();
        $exception = InvalidArgumentException::fromNonExistingClass($className);

        $this->assertInstanceOf('Doctrine\\Instantiator\\Exception\\InvalidArgumentException', $exception);
        $this->assertSame('The provided class "' . $className . '" does not exist', $exception->getMessage());
    }

    public function testFromNonExistingTypeWithTrait()
    {
        if (PHP_VERSION_ID < 50400) {
            $this->markTestSkipped('Need at least PHP 5.4.0, as this test requires traits support to run');
        }

        $exception = InvalidArgumentException::fromNonExistingClass(
            'DoctrineTest\\InstantiatorTestAsset\\SimpleTraitAsset'
        );

        $this->assertSame(
            'The provided type "DoctrineTest\\InstantiatorTestAsset\\SimpleTraitAsset" is a trait, '
            . 'and can not be instantiated',
            $exception->getMessage()
        );
    }

    public function testFromNonExistingTypeWithInterface()
    {
        $exception = InvalidArgumentException::fromNonExistingClass('Doctrine\\Instantiator\\InstantiatorInterface');

        $this->assertSame(
            'The provided type "Doctrine\\Instantiator\\InstantiatorInterface" is an interface, '
            . 'and can not be instantiated',
            $exception->getMessage()
        );
    }

    public function testFromAbstractClass()
    {
        $reflection = new ReflectionClass('DoctrineTest\\InstantiatorTestAsset\\AbstractClassAsset');
        $exception  = InvalidArgumentException::fromAbstractClass($reflection);

        $this->assertSame(
            'The provided class "DoctrineTest\\InstantiatorTestAsset\\AbstractClassAsset" is abstract, '
            . 'and can not be instantiated',
            $exception->getMessage()
        );
    }
}
