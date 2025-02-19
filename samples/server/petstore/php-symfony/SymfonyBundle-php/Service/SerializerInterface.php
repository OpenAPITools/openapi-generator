<?php

namespace OpenAPI\Server\Service;

interface SerializerInterface
{
    /**
     * Serializes the given data to the specified output format.
     *
     * @param object|array|scalar $data
     * @param string $format
     *
     * @return string
     */
    public function serialize($data, string $format): string;

    /**
     * Deserializes the given data to the specified type.
     *
     * @param mixed $data
     * @param string $type
     * @param string $format
     *
     * @return object|array|scalar
     */
    public function deserialize($data, string $type, string $format);
}
