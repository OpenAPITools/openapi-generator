<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\PhpAttribute as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

#[PHA\Route("/pet/{petId}/downloadImage")]
class PetPetIdDownloadImage
{
    /**
     * downloads an image
     */
    #[PHA\Post()]
    // TODO check if producer is valid, if it has correct priority and if it can be moved to class annotation
    #[PHA\Producer("application/zip", PHProducer\Transfer::class)]
    public function downloadFile(ServerRequestInterface $request): \SplFileObject
    {
        //TODO implement method
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
