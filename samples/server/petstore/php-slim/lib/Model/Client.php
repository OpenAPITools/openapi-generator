<?php
/**
 * Client
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * Client
 */
class Client
{

    /** @var string $client (optional) */
    private $client;

    /**
     * Client constructor
     *
     * @param string|null $client (optional)
     */
    public function __construct(
        $client = null
    ) {
        $this->client = $client;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $client = Client::createFromObject([ 'client' => 'foobar' ]);
     *
     * @return Client
     */
    public static function createFromObject(array $data = null)
    {
        $client = (isset($data['client'])) ? $data['client'] : null;
        return new Client(
            $client
        );
    }
}
