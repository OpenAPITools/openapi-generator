<?php

namespace Swagger\Server\Service;

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
    public function serialize($data, $format);

    /**
     * Deserializes the given data to the specified type.
     *
     * @param string $data
     * @param string $type
     * @param string $format
     *
     * @return object|array|scalar
     */
    public function deserialize($data, $type, $format);
}
