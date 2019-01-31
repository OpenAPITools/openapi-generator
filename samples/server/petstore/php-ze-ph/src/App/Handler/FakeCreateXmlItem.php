<?php
declare(strict_types=1);

namespace App\Handler;

use Articus\PathHandler\Annotation as PHA;
use Articus\PathHandler\Consumer as PHConsumer;
use Articus\PathHandler\Producer as PHProducer;
use Articus\PathHandler\Attribute as PHAttribute;
use Articus\PathHandler\Exception as PHException;
use Psr\Http\Message\ServerRequestInterface;

/**
 * @PHA\Route(pattern="/fake/create_xml_item")
 */
class FakeCreateXmlItem
{
    /**
     * creates an XmlItem
     * @PHA\Post()
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/xml")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/xml; charset=utf-8")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="application/xml; charset=utf-16")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="text/xml")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="text/xml; charset=utf-8")
     * TODO check if consumer is valid, if it has correct priority and if it can be moved to class annotation
     * @PHA\Consumer(name=PHConsumer\Json::class, mediaType="text/xml; charset=utf-16")
     * @PHA\Attribute(name=PHAttribute\Transfer::class, options={"type":\App\DTO\XmlItem::class,"objectAttr":"bodyData"})
     * @param ServerRequestInterface $request
     *
     * @throws PHException\HttpCode 501 if the method is not implemented
     */
    public function createXmlItem(ServerRequestInterface $request)
    {
        //TODO implement method
        /** @var \App\DTO\XmlItem $bodyData */
        $bodyData = $request->getAttribute("bodyData");
        throw new PHException\HttpCode(501, "Not implemented");
    }
}
