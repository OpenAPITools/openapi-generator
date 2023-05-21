<?php

namespace OpenAPI\Server\Service;

use JMS\Serializer\SerializerBuilder;
use JMS\Serializer\Naming\CamelCaseNamingStrategy;
use JMS\Serializer\Naming\SerializedNameAnnotationStrategy;
use JMS\Serializer\Serializer;
use JMS\Serializer\Visitor\Factory\XmlDeserializationVisitorFactory;
use DateTime;
use RuntimeException;

class JmsSerializer implements SerializerInterface
{
    protected Serializer $serializer;

    public function __construct()
    {
        $namingStrategy = new SerializedNameAnnotationStrategy(new CamelCaseNamingStrategy());
        $this->serializer = SerializerBuilder::create()
            ->setDeserializationVisitor('json', new StrictJsonDeserializationVisitorFactory())
            ->setDeserializationVisitor('xml', new XmlDeserializationVisitorFactory())
            ->setPropertyNamingStrategy($namingStrategy)
            ->build();
    }

    /**
     * @inheritdoc
     */
    public function serialize($data, string $format): string
    {
        return SerializerBuilder::create()->build()->serialize($data, $this->convertFormat($format));
    }

    /**
     * @inheritdoc
     */
    public function deserialize($data, string $type, string $format)
    {
        if ($format == 'string') {
            return $this->deserializeString($data, $type);
        }

        // If we end up here, let JMS serializer handle the deserialization
        return $this->serializer->deserialize($data, $type, $this->convertFormat($format));
    }

    private function convertFormat(string $format): ?string
    {
        switch ($format) {
            case 'application/json':
                return 'json';
            case 'application/xml':
                return 'xml';
        }

        return null;
    }

    private function deserializeString($data, string $type)
    {
        // Figure out if we have an array format
        if (1 === preg_match('/array<(csv|ssv|tsv|pipes),(int|string)>/i', $type, $matches)) {
            return $this->deserializeArrayString($matches[1], $matches[2], $data);
        }

        switch ($type) {
            case 'int':
            case 'integer':
                if (is_int($data)) {
                    return $data;
                }

                if (is_numeric($data)) {
                    return $data + 0;
                }

                break;
            case 'string':
                break;
            case 'boolean':
            case 'bool':
                if (is_bool($data)) {
                    return $data;
                }

                if (strtolower($data) === 'true') {
                    return true;
                }

                if (strtolower($data) === 'false') {
                    return false;
                }

                break;
            case 'DateTime':
            case '\DateTime':
                return is_null($data) ? null :new DateTime($data);
            default:
                throw new RuntimeException(sprintf("Type %s is unsupported", $type));
        }

        // If we end up here, just return data
        return $data;
    }

    private function deserializeArrayString(string $format, string $type, $data): array
    {
        if (empty($data)) {
            return [];
        }

        // Parse the string using the correct separator
        switch ($format) {
            case 'csv':
                $data = explode(',', $data);
                break;
            case 'ssv':
                $data = explode(' ', $data);
                break;
            case 'tsv':
                $data = explode("\t", $data);
                break;
            case 'pipes':
                $data = explode('|', $data);
                break;
            default;
                $data = [];
        }

        // Deserialize each of the array elements
        foreach ($data as $key => $item) {
            $data[$key] = $this->deserializeString($item, $type);
        }

        return $data;
    }
}
