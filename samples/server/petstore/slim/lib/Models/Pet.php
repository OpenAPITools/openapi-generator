<?php
/*
 * Pet
 */
namespace \Models;

/*
 * Pet
 */
class Pet {
    /* @var int $id  */
    private $id;
/* @var \\Models\Category $category  */
    private $category;
/* @var string $name  */
    private $name;
/* @var string[] $photoUrls  */
    private $photoUrls;
/* @var \\Models\Tag[] $tags  */
    private $tags;
/* @var string $status pet status in the store */
    private $status;
}
