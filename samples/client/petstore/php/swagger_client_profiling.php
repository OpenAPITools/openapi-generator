<?php
require_once(__DIR__ . '/SwaggerClient-php/autoload.php');

// ref/credit: http://stackoverflow.com/a/29022400/677735
// Call this at each point of interest, passing a descriptive string
function prof_flag($str)
{
    global $prof_timing, $prof_names;
    $prof_timing[] = microtime(true);
    $prof_names[] = $str;
}

// Call this when you're done and want to see the results
function prof_print()
{
    global $prof_timing, $prof_names;
    $size = count($prof_timing);
    for($i=0;$i<$size - 1; $i++)
    {
        echo sprintf("%s => %f\n", $prof_names[$i], $prof_timing[$i+1]-$prof_timing[$i]);
    }
    echo "{$prof_names[$size-1]}\n";
}

$counter =  5; // run 5 times by default
$new_pet_id = 50001; // ID of pet that needs to be fetched

for ($x = 0; $x <= $counter; $x++) {
    try {
        prof_flag("$x: NEW PETAPI");
        $pet_api = new Swagger\Client\Api\PetApi();
   
        // ~~~ ADD PET ~~~ 
        prof_flag("$x: ADD PET");
        // add pet (post json)
        $new_pet = new Swagger\Client\Model\Pet;
        $new_pet->setId($new_pet_id);
        $new_pet->setName("profiler");
        $new_pet->setStatus("available");
        $new_pet->setPhotoUrls(array("http://profiler.com"));
        // new tag
        $tag= new Swagger\Client\Model\Tag;
        $tag->setId($new_pet_id); // use the same id as pet
        $tag->setName("profile tag 1");
        // new category
        $category = new Swagger\Client\Model\Category;
        $category->setId($new_pet_id); // use the same id as pet
        $category->setName("profile category 1");
    
        $new_pet->setTags(array($tag));
        $new_pet->setCategory($category);
    
        // add a new pet (model)
        $add_response = $pet_api->addPet($new_pet);
    
        // ~~~ GET PET ~~~ 
        prof_flag("$x: GET PET");
        $response = $pet_api->getPetById($new_pet_id);

        // ~~~ UPDATE PET WITH FORM ~~~ 
        prof_flag("$x: UPDATE PET");
        $response = $pet_api->updatePetWithForm($new_pet_id, "new profiler", "sold");

        // ~~~ DELETE PET ~~~
        prof_flag("$x: DELETE PET");
        $response = $pet_api->deletePet($new_pet_id);

    } catch (Swagger\Client\ApiException $e) {
        echo 'Caught exception: ', $e->getMessage(), "\n";
        echo 'HTTP response headers: ', print_r($e->getResponseHeaders(), true), "\n";
        echo 'HTTP response body: ', print_r($e->getResponseBody(), true), "\n";
        echo 'HTTP status code: ', $e->getCode(), "\n";
    }

}

prof_flag("$x: FINISH");
prof_print();


