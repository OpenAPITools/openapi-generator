<?php
/**
 * Pet
 */
namespace app\Models;

/**
 * Pet
 */
class Pet {

    /** @var int $id */
    public $id = 0;

    /** @var \app\Models\Category $category */
    public $category;

    /** @var string $name */
    public $name = "";

    /** @var string[] $photoUrls */
    public $photoUrls = [];

    /** @var \app\Models\Tag[] $tags */
    public $tags = [];

    /** @var string $status pet status in the store*/
    public $status = "";

}
