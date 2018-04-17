<?php

namespace Swagger\Client;

use Swagger\Client\Api\PetApi;
use Swagger\Client\Model\Pet;

class AsyncTest extends \PHPUnit_Framework_TestCase
{
    /** @var PetApi */
    private $api;

    /** @var  int */
    private $petId;

    public function setUp()
    {
        $this->api = new Api\PetApi();

        $this->petId = 10005;
        $pet = new Model\Pet;
        $pet->setId($this->petId);
        $pet->setName("PHP Unit Test");
        $pet->setPhotoUrls(array("http://test_php_unit_test.com"));
        // new tag
        $tag= new Model\Tag;
        $tag->setId($this->petId); // use the same id as pet
        $tag->setName("test php tag");
        // new category
        $category = new Model\Category;
        $category->setId($this->petId); // use the same id as pet
        $category->setName("test php category");

        $pet->setTags(array($tag));
        $pet->setCategory($category);

        $pet_api = new Api\PetApi();
        // add a new pet (model)
        $add_response = $pet_api->addPet($pet);
    }

    public function testAsyncRequest()
    {
        $promise = $this->api->getPetByIdAsync(10005);

        $promise2 = $this->api->getPetByIdAsync(10005);

        $pet = $promise->wait();
        $pet2 = $promise2->wait();
        $this->assertInstanceOf(Pet::class, $pet);
        $this->assertInstanceOf(Pet::class, $pet2);
    }

    public function testAsyncRequestWithHttpInfo()
    {
        $promise = $this->api->getPetByIdAsyncWithHttpInfo($this->petId);

        list($pet, $status, $headers) = $promise->wait();
        $this->assertEquals(200, $status);
        $this->assertInternalType('array', $headers);
        $this->assertInstanceOf(Pet::class, $pet);
    }

    public function testAsyncThrowingException()
    {
        $this->setExpectedException(ApiException::class);

        $promise = $this->api->getPetByIdAsync(0);
        $promise->wait();
    }

    public function testAsyncApiExceptionWithoutWaitIsNotThrown()
    {
        $promise = $this->api->getPetByIdAsync(0);
        sleep(1);
    }

    public function testAsyncHttpInfoThrowingException()
    {
        $this->setExpectedException(ApiException::class);

        $promise = $this->api->getPetByIdAsyncWithHttpInfo(0);
        $promise->wait();
    }
}
