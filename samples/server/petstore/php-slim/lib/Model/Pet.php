<?php
/**
 * Pet
 */
namespace OpenAPIServer\Model;

/**
 * Pet
 */
class Pet
{
    
    /** @var int $id */
    private $id;
    
    /** @var \OpenAPIServer\Model\Category $category */
    private $category;
    
    /** @var string $name */
    private $name;
    
    /** @var string[] $photoUrls */
    private $photoUrls;
    
    /** @var \OpenAPIServer\Model\Tag[] $tags */
    private $tags;
    
    /** @var string $status pet status in the store*/
    private $status;
}
