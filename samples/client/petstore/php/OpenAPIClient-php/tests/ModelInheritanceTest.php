<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Model\Animal;
use OpenAPI\Client\Model\Cat;
use OpenAPI\Client\Model\Dog;
use PHPUnit\Framework\TestCase;

/**
 * Test that Dog properly inherit Animal
 *
 * @package OpenAPI\Client
 */
class ModelInheritanceTest extends TestCase
{
    /**
     * test if default values works
     */
    public function testDefaultValues()
    {
        // add some animals to the farm to make sure the ArrayAccess
        // interface works
        $dog = new Dog();
        $animal = new Animal();

        // assert we can look up the animals in the farm by array
        // indices (let's try a random order)
        $this->assertSame('red', $dog->getColor());
        $this->assertSame('red', $animal->getColor());
    }

    /**
     * test inheritance in the model
     */
    public function testInheritance()
    {
        $newDog = new Dog;
        // the object should be an instance of the derived class
        $this->assertInstanceOf(Dog::class, $newDog);
        // the object should also be an instance of the parent class
        $this->assertInstanceOf(Animal::class, $newDog);
    }

    /**
     * test inheritance constructor is working with data initialization
     */
    public function testInheritanceConstructorDataInitialization()
    {
        // initialize the object with data in the constructor
        $data = [
            'class_name' => 'Dog',
            'breed' => 'Great Dane',
        ];
        $newDog = new Dog($data);

        // the property on the derived class should be set
        $this->assertSame('Great Dane', $newDog->getBreed());
        // the property on the parent class should be set
        $this->assertSame('Dog', $newDog->getClassName());
    }

    /**
     * test if discriminator is initialized automatically
     */
    public function testDiscriminatorInitialization()
    {
        $newDog = new Dog();
        $this->assertSame('Dog', $newDog->getClassName());
    }

    /**
     * test if ArrayAccess interface works
     */
    public function testArrayStuff()
    {
        // create an array of Animal
        $farm = array();

        // add some animals to the farm to make sure the ArrayAccess interface works
        $farm[] = new Dog();
        $farm[] = new Cat();
        $farm[] = new Animal();

        // assert we can look up the animals in the farm by array indices (let's try a random order)
        $this->assertInstanceOf(Cat::class, $farm[1]);
        $this->assertInstanceOf(Dog::class, $farm[0]);
        $this->assertInstanceOf(Animal::class, $farm[2]);

        // let's try to `foreach` the animals in the farm and let's try to use the objects we loop through
        foreach ($farm as $animal) {
            $this->assertContains($animal->getClassName(), ['Dog', 'Cat', 'Animal']);
            $this->assertInstanceOf('OpenAPI\Client\Model\Animal', $animal);
        }
    }
}
