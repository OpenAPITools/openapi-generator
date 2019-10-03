<?php

namespace OpenAPI\Client;

use OpenAPI\Client\Api\PetApi;
use OpenAPI\Client\Api\StoreApi;
use OpenAPI\Client\Model\Category;
use OpenAPI\Client\Model\Pet;
use OpenAPI\Client\Model\Tag;
use PHPUnit\Framework\TestCase;

class StoreApiTest extends TestCase
{
    /** @var  StoreApi */
    private $api;

    public function setUp()
    {
        $this->api = new StoreApi();
    }

    /**
     * Setup before running each test case
     */
    public static function setUpBeforeClass()
    {
        // add a new pet (id 10005) to ensure the pet object is available for all the tests
        // new pet
        $id = 10005;
        $pet = new Pet();
        $pet->setId($id);
        $pet->setName('PHP Unit Test');
        $pet->setStatus('available');
        // new tag
        $tag = new Tag();
        $tag->setId($id); // use the same id as pet
        $tag->setName('test php tag');
        // new category
        $category = new Category();
        $category->setId($id); // use the same id as pet
        $category->setName('test php category');

        $pet->setTags([$tag]);
        $pet->setCategory($category);

        $api = new PetApi();
        $api->addPet($pet);
    }

    public function testGetInventory()
    {
        $result = $this->api->getInventory();

        $this->assertInternalType('array', $result);
        $this->assertInternalType('int', $result['available']);
    }
}
