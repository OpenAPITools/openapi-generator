<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/pet/{petId}")]
class PetPetId
{
    /**
     * Deletes a pet
     */
    #[PHA\Delete()]
    public function deletePet(ServerRequestInterface $request)
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Find pet by ID
     */
    #[PHA\Get()]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/xml", PHProducer\Transfer::class)]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/json", PHProducer\Transfer::class)]
    public function getPetById(ServerRequestInterface $request): \App\DTO\Pet
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
    /**
     * Updates a pet in the store with form data
     */
    #[PHA\Post()]
    public function updatePetWithForm(ServerRequestInterface $request)
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
