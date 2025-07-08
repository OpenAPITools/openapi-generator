<?php

namespace OpenAPI\Client;

use DateTime;
use PHPUnit\Framework\TestCase;
use SplFileObject;

/**
 * class FormDataProcessorTest
 *
 * @package OpenAPI\Client
 */
class FormDataProcessorTest extends TestCase
{
    /**
     * @dataProvider providerFlatten
     */
    public function testFlatten($data, array $expected): void
    {
        $formDataProcessor = new FormDataProcessor();
        $formData = $formDataProcessor->prepare($data);

        $result = $formDataProcessor::flatten($formData);

        $this->assertEquals($expected, $result);
    }

    public function providerFlatten(): iterable
    {
        $data = [
            'id'         => '1234',
            'name'       => 'Spike',
            'photo_urls' => [
                'https://example.com/picture_1.jpg',
                'https://example.com/picture_2.jpg',
            ],
            'status'     => Model\Pet::STATUS_AVAILABLE,
            'category'   => [
                'id'   => '12345',
                'name' => 'Category_Name',
            ],
            'tags'       => [
                [
                    'id'   => '12345',
                    'name' => 'tag_1',
                ],
                [
                    'id'   => '98765',
                    'name' => 'tag_2',
                ],
            ],
        ];

        yield [
            'data'     => $data,
            'expected' => [
                'id'             => $data['id'],
                'name'           => $data['name'],
                'photo_urls[0]'  => $data['photo_urls'][0],
                'photo_urls[1]'  => $data['photo_urls'][1],
                'status'         => $data['status'],
                'category[id]'   => (string) $data['category']['id'],
                'category[name]' => $data['category']['name'],
                'tags[0][id]'    => (string) $data['tags'][0]['id'],
                'tags[0][name]'  => $data['tags'][0]['name'],
                'tags[1][id]'    => (string) $data['tags'][1]['id'],
                'tags[1][name]'  => $data['tags'][1]['name'],
            ],
        ];

        $category = (new Model\Category())
            ->setId($data['category']['id'])
            ->setName($data['category']['name']);

        $tags_1 = (new Model\Tag())
            ->setId($data['tags'][0]['id'])
            ->setName($data['tags'][0]['name']);

        $tags_2 = (new Model\Tag())
            ->setId($data['tags'][1]['id'])
            ->setName($data['tags'][1]['name']);

        $tags = [
            $tags_1,
            $tags_2,
        ];

        $pet = new Model\Pet([]);
        $pet->setId($data['id'])
            ->setName($data['name'])
            ->setPhotoUrls($data['photo_urls'])
            ->setStatus($data['status'])
            ->setCategory($category)
            ->setTags($tags);

        yield [
            'data'     => ['pet' => $pet],
            'expected' => [
                'pet[id]'             => $data['id'],
                'pet[name]'           => $data['name'],
                'pet[photo_urls][0]'  => $data['photo_urls'][0],
                'pet[photo_urls][1]'  => $data['photo_urls'][1],
                'pet[status]'         => $data['status'],
                'pet[category][id]'   => (string) $data['category']['id'],
                'pet[category][name]' => $data['category']['name'],
                'pet[tags][0][id]'    => (string) $data['tags'][0]['id'],
                'pet[tags][0][name]'  => $data['tags'][0]['name'],
                'pet[tags][1][id]'    => (string) $data['tags'][1]['id'],
                'pet[tags][1][name]'  => $data['tags'][1]['name'],
            ],
        ];

        yield [
            'data'     => ['nested' => ['pet' => $pet]],
            'expected' => [
                'nested[pet][id]'             => $data['id'],
                'nested[pet][name]'           => $data['name'],
                'nested[pet][photo_urls][0]'  => $data['photo_urls'][0],
                'nested[pet][photo_urls][1]'  => $data['photo_urls'][1],
                'nested[pet][status]'         => $data['status'],
                'nested[pet][category][id]'   => (string) $data['category']['id'],
                'nested[pet][category][name]' => $data['category']['name'],
                'nested[pet][tags][0][id]'    => (string) $data['tags'][0]['id'],
                'nested[pet][tags][0][name]'  => $data['tags'][0]['name'],
                'nested[pet][tags][1][id]'    => (string) $data['tags'][1]['id'],
                'nested[pet][tags][1][name]'  => $data['tags'][1]['name'],
            ],
        ];

        yield [
            'data'     => ['key' => new DateTime('2021-10-06T20:17:16')],
            'expected' => ['key' => '2021-10-06T20:17:16+00:00'],
        ];

        yield [
            'data'     => ['key' => true],
            'expected' => ['key' => 'true'],
        ];

        yield [
            'data'     => ['key' => false],
            'expected' => ['key' => 'false'],
        ];

        yield [
            'data'     => ['key' => 'some value'],
            'expected' => ['key' => 'some value'],
        ];
    }

    public function testNullValueIgnored(): void
    {
        $data = [
            'id'         => '1234',
            'name'       => 'Spike',
            'photo_urls' => null,
            'status'     => null,
            'category'   => null,
            'tags'       => null,
        ];

        $expected = [
            'id'   => $data['id'],
            'name' => $data['name'],
        ];

        $formDataProcessor = new FormDataProcessor();
        $formData = $formDataProcessor->prepare($data);

        $result = $formDataProcessor::flatten($formData);

        $this->assertEquals($expected, $result);
    }

    public function testHasFile(): void
    {
        $filepath = realpath(__DIR__ . '/../.openapi-generator/VERSION');
        $file = new SplFileObject($filepath);

        $pet = new Model\PetWithFile([]);
        $pet->setId(123)
            ->setName('Spike')
            ->setFile($file);

        $formDataProcessor = new FormDataProcessor();

        $this->assertFalse($formDataProcessor->has_file);
        $formData = $formDataProcessor->prepare(['pet' => $pet]);

        $this->assertIsResource($formData['pet']['file'][0]);
        $this->assertTrue($formDataProcessor->has_file);
    }

    public function testHasFileNested(): void
    {
        $filepath = realpath(__DIR__ . '/../.openapi-generator/VERSION');
        $file = new SplFileObject($filepath);

        $pet = new Model\PetWithFile([]);
        $pet->setId(123)
            ->setName('Spike')
            ->setFile($file);

        $formDataProcessor = new FormDataProcessor();

        $this->assertFalse($formDataProcessor->has_file);
        $formData = $formDataProcessor->prepare(['nested' => ['pet' => $pet]]);

        $this->assertIsResource($formData['nested']['pet']['file'][0]);
        $this->assertTrue($formDataProcessor->has_file);
    }

    public function testHasFileMultiple(): void
    {
        $filepath = realpath(__DIR__ . '/../.openapi-generator/VERSION');
        $file = new SplFileObject($filepath);

        $pet = new Model\PetWithFile([]);
        $pet->setId(123)
            ->setName('Spike')
            ->setMultipleFiles([$file]);

        $formDataProcessor = new FormDataProcessor();

        $this->assertFalse($formDataProcessor->has_file);
        $formData = $formDataProcessor->prepare(['pet' => $pet]);

        $this->assertIsResource($formData['pet']['multiple_files'][0]);
        $this->assertTrue($formDataProcessor->has_file);
    }

    public function testHasFileMultipleNested(): void
    {
        $filepath = realpath(__DIR__ . '/../.openapi-generator/VERSION');
        $file = new SplFileObject($filepath);

        $pet = new Model\PetWithFile([]);
        $pet->setId(123)
            ->setName('Spike')
            ->setMultipleFiles([$file]);

        $formDataProcessor = new FormDataProcessor();

        $this->assertFalse($formDataProcessor->has_file);
        $formData = $formDataProcessor->prepare(['nested' => ['pet' => $pet]]);

        $this->assertIsResource($formData['nested']['pet']['multiple_files'][0]);
        $this->assertTrue($formDataProcessor->has_file);
    }
}
