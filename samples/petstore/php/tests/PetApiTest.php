<?php

require_once 'BaseApiTest.php';

class PetApiTest extends BaseApiTest {

  // Choose a random id for new pet insertion
  public static $randomId;

  public static function setUpBeforeClass() {
    self::$randomId = rand(10000, 100000);
    print "Pet id is " . self::$randomId . "\n\n";
  }

  public function testPetApis() {
    $ch = curl_init("http://petstore.swagger.wordnik.com/api/pet.json");
    if (! $ch) {
    	die("No php curl handle");
    }
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    $data = curl_exec($ch);
    $doc = json_decode($data);

    $this->assertEquals(3, count($doc->apis));
  }

  public function testPetApisWithKey() {
    $ch = curl_init("http://petstore.swagger.wordnik.com/api/pet.json?api_key=special-key");
    if (! $ch) {
      die("No php curl handle");
    }
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);

    $data = curl_exec($ch);
    $doc = json_decode($data);

    $this->assertEquals(4, count($doc->apis));
  }

  public function testGetPetById() {
    $res = $this->petApi->getPetById(1);
    $this->assertEquals($res->id, 1);
  }

  public function testAddPet() {
    $pet = new Pet();
    $pet->id = self::$randomId;
    $tag1 = new Tag();
    $tag1->name = "tag1";
    $tag2 = new Tag();
    $tag2->name = "some tag";
    $pet->tags = array($tag1, $tag2);
    $category = new Category();
    $category->name = "Cats";
    $pet->category = $category;
    $pet->status = "sold";
    $pet->name = "Shermie";
    $pet->photoUrls = array("http://foo.com/1.jpg", "http://foo.com/1.jpg");
    $res = $this->petApi->addPet($pet);

    $new_pet = $this->petApi->getPetById($pet->id);

    $this->assertEquals($new_pet->id, $pet->id);
    $this->assertEquals($new_pet->name, $pet->name);
    $this->assertEquals($new_pet->tags, $pet->tags);
    $this->assertEquals($new_pet->status, $pet->status);
    $this->assertEquals($new_pet->category, $pet->category);
    $this->assertEquals($new_pet->photoUrls, $pet->photoUrls);
  }

  public function testUpdatePet() {
    $pet = new Pet();
    $pet->id = self::$randomId;
    $tag1 = new Tag();
    $tag1->name = rand(10000, 100000);
    $tag2 = new Tag();
    $tag2->name = "special-tag";
    $pet->tags = array($tag1, $tag2);
    $category = new Category();
    $category->name = rand(10000, 100000);
    $pet->category = $category;
    $pet->status = "sold";
    $pet->name = rand(10000, 100000);
    $pet->photoUrls = array(rand(10000, 100000), rand(10000, 100000));
    $res = $this->petApi->updatePet($pet);

    $updated_pet = $this->petApi->getPetById($pet->id);

    $this->assertEquals($updated_pet->id, $pet->id);
    $this->assertEquals($updated_pet->name, $pet->name);
    $this->assertEquals($updated_pet->tags, $pet->tags);
    $this->assertEquals($updated_pet->status, $pet->status);
    $this->assertEquals($updated_pet->category, $pet->category);
    $this->assertEquals($updated_pet->photoUrls, $pet->photoUrls);  }

  public function testFindPetsByTags() {
    $res = $this->petApi->findPetsByTags("special-tag");
    $tag_found = false;
    foreach ($res as $found_pet) {
      if ($found_pet->id == self::$randomId) {
        $tag_found = true;
      }
    }
    $this->assertEquals(true, $tag_found);
  }

  public function testFindPetsByStatus() {
    $res = $this->petApi->findPetsByStatus("sold");
    $tag_found = false;
    foreach ($res as $found_pet) {
      if ($found_pet->id == self::$randomId) {
        $tag_found = true;
      }
    }
    $this->assertEquals(true, $tag_found);
  }




}
?>