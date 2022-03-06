<?php

declare(strict_types=1);

namespace OpenAPI\Server\Service;

use JMS\Serializer\Visitor\DeserializationVisitorInterface;
use JMS\Serializer\Visitor\Factory\DeserializationVisitorFactory;

final class StrictJsonDeserializationVisitorFactory implements DeserializationVisitorFactory
{
    /**
     * @var int
     */
    private $options = 0;

    /**
     * @var int
     */
    private $depth = 512;

    public function getVisitor(): DeserializationVisitorInterface
    {
        return new StrictJsonDeserializationVisitor($this->options, $this->depth);
    }

    public function setOptions(int $options): self
    {
        $this->options = $options;

        return $this;
    }

    public function setDepth(int $depth): self
    {
        $this->depth = $depth;

        return $this;
    }
}
