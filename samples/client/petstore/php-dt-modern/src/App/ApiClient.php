<?php
declare(strict_types=1);

namespace App;

use Articus\DataTransfer as DT;
use OpenAPIGenerator\APIClient as OAGAC;
use Psr\Http\Client\ClientExceptionInterface;
use Psr\Http\Message\ResponseInterface;

/**
 * OpenAPI Petstore
 * This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.
 * The version of the OpenAPI document: 1.0.0
 */
class ApiClient extends OAGAC\AbstractApiClient
{
    //region addPet
    /**
     * Add a new pet to the store
     * @param \App\DTO\Pet $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function addPetRaw(
        \App\DTO\Pet $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/pet', [], []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Add a new pet to the store
     * @param \App\DTO\Pet $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function addPet(
        \App\DTO\Pet $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->addPetRaw($requestContent, $security, $requestMediaType, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Pet();
                break;
            case 405:
                /* Invalid input */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Add a new pet to the store
     * @param \App\DTO\Pet $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return \App\DTO\Pet
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function addPetResult(
        \App\DTO\Pet $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Pet
    {
        return $this->getSuccessfulContent(...$this->addPet($requestContent, $security, $requestMediaType, $responseMediaType));
    }
    //endregion

    //region createUser
    /**
     * Create user
     * @param \App\DTO\User $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function createUserRaw(
        \App\DTO\User $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/user', [], []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Create user
     * @param \App\DTO\User $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function createUser(
        \App\DTO\User $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): array
    {
        $response = $this->createUserRaw($requestContent, $security, $requestMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            default:
                /* successful operation */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Create user
     * @param \App\DTO\User $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function createUserResult(
        \App\DTO\User $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->createUser($requestContent, $security, $requestMediaType));
    }
    //endregion

    //region createUsersWithArrayInput
    /**
     * Creates list of users with given input array
     * @param \App\DTO\Collection36 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function createUsersWithArrayInputRaw(
        \App\DTO\Collection36 $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/user/createWithArray', [], []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Creates list of users with given input array
     * @param \App\DTO\Collection36 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function createUsersWithArrayInput(
        \App\DTO\Collection36 $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): array
    {
        $response = $this->createUsersWithArrayInputRaw($requestContent, $security, $requestMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            default:
                /* successful operation */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Creates list of users with given input array
     * @param \App\DTO\Collection36 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function createUsersWithArrayInputResult(
        \App\DTO\Collection36 $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->createUsersWithArrayInput($requestContent, $security, $requestMediaType));
    }
    //endregion

    //region createUsersWithListInput
    /**
     * Creates list of users with given input array
     * @param \App\DTO\Collection36 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function createUsersWithListInputRaw(
        \App\DTO\Collection36 $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/user/createWithList', [], []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Creates list of users with given input array
     * @param \App\DTO\Collection36 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function createUsersWithListInput(
        \App\DTO\Collection36 $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): array
    {
        $response = $this->createUsersWithListInputRaw($requestContent, $security, $requestMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            default:
                /* successful operation */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Creates list of users with given input array
     * @param \App\DTO\Collection36 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function createUsersWithListInputResult(
        \App\DTO\Collection36 $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->createUsersWithListInput($requestContent, $security, $requestMediaType));
    }
    //endregion

    //region deleteOrder
    /**
     * Delete purchase order by ID
     * @param \App\DTO\DeleteOrderParameterData $parameters
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function deleteOrderRaw(
        \App\DTO\DeleteOrderParameterData $parameters
    ): ResponseInterface
    {
        $request = $this->createRequest('DELETE', '/store/order/{orderId}', $this->getPathParameters($parameters), []);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Delete purchase order by ID
     * @param \App\DTO\DeleteOrderParameterData $parameters
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function deleteOrder(
        \App\DTO\DeleteOrderParameterData $parameters
    ): array
    {
        $response = $this->deleteOrderRaw($parameters);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 400:
                /* Invalid ID supplied */
                break;
            case 404:
                /* Order not found */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Delete purchase order by ID
     * @param \App\DTO\DeleteOrderParameterData $parameters
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function deleteOrderResult(
        \App\DTO\DeleteOrderParameterData $parameters
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->deleteOrder($parameters));
    }
    //endregion

    //region deletePet
    /**
     * Deletes a pet
     * @param \App\DTO\DeletePetParameterData $parameters
     * @param iterable|string[][] $security
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function deletePetRaw(
        \App\DTO\DeletePetParameterData $parameters,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]]
    ): ResponseInterface
    {
        $request = $this->createRequest('DELETE', '/pet/{petId}', $this->getPathParameters($parameters), []);
        $request = $this->addCustomHeaders($request, $parameters);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Deletes a pet
     * @param \App\DTO\DeletePetParameterData $parameters
     * @param iterable|string[][] $security
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function deletePet(
        \App\DTO\DeletePetParameterData $parameters,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]]
    ): array
    {
        $response = $this->deletePetRaw($parameters, $security);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 400:
                /* Invalid pet value */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Deletes a pet
     * @param \App\DTO\DeletePetParameterData $parameters
     * @param iterable|string[][] $security
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function deletePetResult(
        \App\DTO\DeletePetParameterData $parameters,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]]
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->deletePet($parameters, $security));
    }
    //endregion

    //region deleteUser
    /**
     * Delete user
     * @param \App\DTO\DeleteUserParameterData $parameters
     * @param iterable|string[][] $security
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function deleteUserRaw(
        \App\DTO\DeleteUserParameterData $parameters,
        iterable $security = ['api_key' => []]
    ): ResponseInterface
    {
        $request = $this->createRequest('DELETE', '/user/{username}', $this->getPathParameters($parameters), []);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Delete user
     * @param \App\DTO\DeleteUserParameterData $parameters
     * @param iterable|string[][] $security
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function deleteUser(
        \App\DTO\DeleteUserParameterData $parameters,
        iterable $security = ['api_key' => []]
    ): array
    {
        $response = $this->deleteUserRaw($parameters, $security);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 400:
                /* Invalid username supplied */
                break;
            case 404:
                /* User not found */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Delete user
     * @param \App\DTO\DeleteUserParameterData $parameters
     * @param iterable|string[][] $security
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function deleteUserResult(
        \App\DTO\DeleteUserParameterData $parameters,
        iterable $security = ['api_key' => []]
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->deleteUser($parameters, $security));
    }
    //endregion

    //region findPetsByStatus
    /**
     * Finds Pets by status
     * @param \App\DTO\FindPetsByStatusParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function findPetsByStatusRaw(
        \App\DTO\FindPetsByStatusParameterData $parameters,
        iterable $security = ['petstore_auth' => ['read:pets', ]],
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/pet/findByStatus', [], $this->getQueryParameters($parameters));
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Finds Pets by status
     * @param \App\DTO\FindPetsByStatusParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function findPetsByStatus(
        \App\DTO\FindPetsByStatusParameterData $parameters,
        iterable $security = ['petstore_auth' => ['read:pets', ]],
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->findPetsByStatusRaw($parameters, $security, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Collection19();
                break;
            case 400:
                /* Invalid status value */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Finds Pets by status
     * @param \App\DTO\FindPetsByStatusParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return \App\DTO\Collection19
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function findPetsByStatusResult(
        \App\DTO\FindPetsByStatusParameterData $parameters,
        iterable $security = ['petstore_auth' => ['read:pets', ]],
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Collection19
    {
        return $this->getSuccessfulContent(...$this->findPetsByStatus($parameters, $security, $responseMediaType));
    }
    //endregion

    //region findPetsByTags
    /**
     * Finds Pets by tags
     * @param \App\DTO\FindPetsByTagsParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function findPetsByTagsRaw(
        \App\DTO\FindPetsByTagsParameterData $parameters,
        iterable $security = ['petstore_auth' => ['read:pets', ]],
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/pet/findByTags', [], $this->getQueryParameters($parameters));
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Finds Pets by tags
     * @param \App\DTO\FindPetsByTagsParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function findPetsByTags(
        \App\DTO\FindPetsByTagsParameterData $parameters,
        iterable $security = ['petstore_auth' => ['read:pets', ]],
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->findPetsByTagsRaw($parameters, $security, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Collection26();
                break;
            case 400:
                /* Invalid tag value */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Finds Pets by tags
     * @param \App\DTO\FindPetsByTagsParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return \App\DTO\Collection26
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function findPetsByTagsResult(
        \App\DTO\FindPetsByTagsParameterData $parameters,
        iterable $security = ['petstore_auth' => ['read:pets', ]],
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Collection26
    {
        return $this->getSuccessfulContent(...$this->findPetsByTags($parameters, $security, $responseMediaType));
    }
    //endregion

    //region getInventory
    /**
     * Returns pet inventories by status
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function getInventoryRaw(
        iterable $security = ['api_key' => []],
        string $responseMediaType = 'application/json'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/store/inventory', [], []);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Returns pet inventories by status
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function getInventory(
        iterable $security = ['api_key' => []],
        string $responseMediaType = 'application/json'
    ): array
    {
        $response = $this->getInventoryRaw($security, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Collection34();
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Returns pet inventories by status
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return \App\DTO\Collection34
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function getInventoryResult(
        iterable $security = ['api_key' => []],
        string $responseMediaType = 'application/json'
    ): \App\DTO\Collection34
    {
        return $this->getSuccessfulContent(...$this->getInventory($security, $responseMediaType));
    }
    //endregion

    //region getOrderById
    /**
     * Find purchase order by ID
     * @param \App\DTO\GetOrderByIdParameterData $parameters
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function getOrderByIdRaw(
        \App\DTO\GetOrderByIdParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/store/order/{orderId}', $this->getPathParameters($parameters), []);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Find purchase order by ID
     * @param \App\DTO\GetOrderByIdParameterData $parameters
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function getOrderById(
        \App\DTO\GetOrderByIdParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->getOrderByIdRaw($parameters, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Order();
                break;
            case 400:
                /* Invalid ID supplied */
                break;
            case 404:
                /* Order not found */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Find purchase order by ID
     * @param \App\DTO\GetOrderByIdParameterData $parameters
     * @param string $responseMediaType
     * @return \App\DTO\Order
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function getOrderByIdResult(
        \App\DTO\GetOrderByIdParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Order
    {
        return $this->getSuccessfulContent(...$this->getOrderById($parameters, $responseMediaType));
    }
    //endregion

    //region getPetById
    /**
     * Find pet by ID
     * @param \App\DTO\GetPetByIdParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function getPetByIdRaw(
        \App\DTO\GetPetByIdParameterData $parameters,
        iterable $security = ['api_key' => []],
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/pet/{petId}', $this->getPathParameters($parameters), []);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Find pet by ID
     * @param \App\DTO\GetPetByIdParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function getPetById(
        \App\DTO\GetPetByIdParameterData $parameters,
        iterable $security = ['api_key' => []],
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->getPetByIdRaw($parameters, $security, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Pet();
                break;
            case 400:
                /* Invalid ID supplied */
                break;
            case 404:
                /* Pet not found */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Find pet by ID
     * @param \App\DTO\GetPetByIdParameterData $parameters
     * @param iterable|string[][] $security
     * @param string $responseMediaType
     * @return \App\DTO\Pet
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function getPetByIdResult(
        \App\DTO\GetPetByIdParameterData $parameters,
        iterable $security = ['api_key' => []],
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Pet
    {
        return $this->getSuccessfulContent(...$this->getPetById($parameters, $security, $responseMediaType));
    }
    //endregion

    //region getUserByName
    /**
     * Get user by user name
     * @param \App\DTO\GetUserByNameParameterData $parameters
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function getUserByNameRaw(
        \App\DTO\GetUserByNameParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/user/{username}', $this->getPathParameters($parameters), []);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Get user by user name
     * @param \App\DTO\GetUserByNameParameterData $parameters
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function getUserByName(
        \App\DTO\GetUserByNameParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->getUserByNameRaw($parameters, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\User();
                break;
            case 400:
                /* Invalid username supplied */
                break;
            case 404:
                /* User not found */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Get user by user name
     * @param \App\DTO\GetUserByNameParameterData $parameters
     * @param string $responseMediaType
     * @return \App\DTO\User
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function getUserByNameResult(
        \App\DTO\GetUserByNameParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): \App\DTO\User
    {
        return $this->getSuccessfulContent(...$this->getUserByName($parameters, $responseMediaType));
    }
    //endregion

    //region loginUser
    /**
     * Logs user into the system
     * @param \App\DTO\LoginUserParameterData $parameters
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function loginUserRaw(
        \App\DTO\LoginUserParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/user/login', [], $this->getQueryParameters($parameters));
        $request = $this->addAcceptHeader($request, $responseMediaType);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Logs user into the system
     * @param \App\DTO\LoginUserParameterData $parameters
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function loginUser(
        \App\DTO\LoginUserParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->loginUserRaw($parameters, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                break;
            case 400:
                /* Invalid username/password supplied */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Logs user into the system
     * @param \App\DTO\LoginUserParameterData $parameters
     * @param string $responseMediaType
     * @return string
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function loginUserResult(
        \App\DTO\LoginUserParameterData $parameters,
        string $responseMediaType = 'application/xml'
    ): string
    {
        return $this->getSuccessfulContent(...$this->loginUser($parameters, $responseMediaType));
    }
    //endregion

    //region logoutUser
    /**
     * Logs out current logged in user session
     * @param iterable|string[][] $security
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function logoutUserRaw(
        iterable $security = ['api_key' => []]
    ): ResponseInterface
    {
        $request = $this->createRequest('GET', '/user/logout', [], []);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Logs out current logged in user session
     * @param iterable|string[][] $security
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function logoutUser(
        iterable $security = ['api_key' => []]
    ): array
    {
        $response = $this->logoutUserRaw($security);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            default:
                /* successful operation */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Logs out current logged in user session
     * @param iterable|string[][] $security
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function logoutUserResult(
        iterable $security = ['api_key' => []]
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->logoutUser($security));
    }
    //endregion

    //region placeOrder
    /**
     * Place an order for a pet
     * @param \App\DTO\Order $requestContent
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function placeOrderRaw(
        \App\DTO\Order $requestContent,
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/store/order', [], []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Place an order for a pet
     * @param \App\DTO\Order $requestContent
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function placeOrder(
        \App\DTO\Order $requestContent,
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->placeOrderRaw($requestContent, $requestMediaType, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Order();
                break;
            case 400:
                /* Invalid Order */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Place an order for a pet
     * @param \App\DTO\Order $requestContent
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return \App\DTO\Order
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function placeOrderResult(
        \App\DTO\Order $requestContent,
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Order
    {
        return $this->getSuccessfulContent(...$this->placeOrder($requestContent, $requestMediaType, $responseMediaType));
    }
    //endregion

    //region updatePet
    /**
     * Update an existing pet
     * @param \App\DTO\Pet $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function updatePetRaw(
        \App\DTO\Pet $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): ResponseInterface
    {
        $request = $this->createRequest('PUT', '/pet', [], []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Update an existing pet
     * @param \App\DTO\Pet $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function updatePet(
        \App\DTO\Pet $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): array
    {
        $response = $this->updatePetRaw($requestContent, $security, $requestMediaType, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\Pet();
                break;
            case 400:
                /* Invalid ID supplied */
                break;
            case 404:
                /* Pet not found */
                break;
            case 405:
                /* Validation exception */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Update an existing pet
     * @param \App\DTO\Pet $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return \App\DTO\Pet
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function updatePetResult(
        \App\DTO\Pet $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/json',
        string $responseMediaType = 'application/xml'
    ): \App\DTO\Pet
    {
        return $this->getSuccessfulContent(...$this->updatePet($requestContent, $security, $requestMediaType, $responseMediaType));
    }
    //endregion

    //region updatePetWithForm
    /**
     * Updates a pet in the store with form data
     * @param \App\DTO\UpdatePetWithFormParameterData $parameters
     * @param \App\DTO\InlineObject $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function updatePetWithFormRaw(
        \App\DTO\UpdatePetWithFormParameterData $parameters,
        \App\DTO\InlineObject $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/x-www-form-urlencoded'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/pet/{petId}', $this->getPathParameters($parameters), []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Updates a pet in the store with form data
     * @param \App\DTO\UpdatePetWithFormParameterData $parameters
     * @param \App\DTO\InlineObject $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function updatePetWithForm(
        \App\DTO\UpdatePetWithFormParameterData $parameters,
        \App\DTO\InlineObject $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/x-www-form-urlencoded'
    ): array
    {
        $response = $this->updatePetWithFormRaw($parameters, $requestContent, $security, $requestMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 405:
                /* Invalid input */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Updates a pet in the store with form data
     * @param \App\DTO\UpdatePetWithFormParameterData $parameters
     * @param \App\DTO\InlineObject $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function updatePetWithFormResult(
        \App\DTO\UpdatePetWithFormParameterData $parameters,
        \App\DTO\InlineObject $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'application/x-www-form-urlencoded'
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->updatePetWithForm($parameters, $requestContent, $security, $requestMediaType));
    }
    //endregion

    //region updateUser
    /**
     * Updated user
     * @param \App\DTO\UpdateUserParameterData $parameters
     * @param \App\DTO\User $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function updateUserRaw(
        \App\DTO\UpdateUserParameterData $parameters,
        \App\DTO\User $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): ResponseInterface
    {
        $request = $this->createRequest('PUT', '/user/{username}', $this->getPathParameters($parameters), []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * Updated user
     * @param \App\DTO\UpdateUserParameterData $parameters
     * @param \App\DTO\User $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function updateUser(
        \App\DTO\UpdateUserParameterData $parameters,
        \App\DTO\User $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): array
    {
        $response = $this->updateUserRaw($parameters, $requestContent, $security, $requestMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 400:
                /* Invalid user supplied */
                break;
            case 404:
                /* User not found */
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * Updated user
     * @param \App\DTO\UpdateUserParameterData $parameters
     * @param \App\DTO\User $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @return mixed
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function updateUserResult(
        \App\DTO\UpdateUserParameterData $parameters,
        \App\DTO\User $requestContent,
        iterable $security = ['api_key' => []],
        string $requestMediaType = 'application/json'
    ): mixed
    {
        return $this->getSuccessfulContent(...$this->updateUser($parameters, $requestContent, $security, $requestMediaType));
    }
    //endregion

    //region uploadFile
    /**
     * uploads an image
     * @param \App\DTO\UploadFileParameterData $parameters
     * @param \App\DTO\InlineObject1 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return ResponseInterface
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     */
    public function uploadFileRaw(
        \App\DTO\UploadFileParameterData $parameters,
        \App\DTO\InlineObject1 $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'multipart/form-data',
        string $responseMediaType = 'application/json'
    ): ResponseInterface
    {
        $request = $this->createRequest('POST', '/pet/{petId}/uploadImage', $this->getPathParameters($parameters), []);
        $request = $this->addBody($request, $requestMediaType, $requestContent);
        $request = $this->addAcceptHeader($request, $responseMediaType);
        $request = $this->addSecurity($request, $security);
        return $this->httpClient->sendRequest($request);
    }

    /**
     * uploads an image
     * @param \App\DTO\UploadFileParameterData $parameters
     * @param \App\DTO\InlineObject1 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return array
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     */
    public function uploadFile(
        \App\DTO\UploadFileParameterData $parameters,
        \App\DTO\InlineObject1 $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'multipart/form-data',
        string $responseMediaType = 'application/json'
    ): array
    {
        $response = $this->uploadFileRaw($parameters, $requestContent, $security, $requestMediaType, $responseMediaType);
        $responseContent = null;
        switch ($response->getStatusCode())
        {
            case 200:
                /* successful operation */
                $responseContent = new \App\DTO\ApiResponse();
                break;
        }
        $this->parseBody($response, $responseContent);
        return [$responseContent, $response->getHeaders(), $response->getStatusCode(), $response->getReasonPhrase()];
    }

    /**
     * uploads an image
     * @param \App\DTO\UploadFileParameterData $parameters
     * @param \App\DTO\InlineObject1 $requestContent
     * @param iterable|string[][] $security
     * @param string $requestMediaType
     * @param string $responseMediaType
     * @return \App\DTO\ApiResponse
     * @throws ClientExceptionInterface
     * @throws DT\Exception\InvalidData
     * @throws OAGAC\Exception\InvalidResponseBodySchema
     * @throws OAGAC\Exception\UnsuccessfulResponse
     */
    public function uploadFileResult(
        \App\DTO\UploadFileParameterData $parameters,
        \App\DTO\InlineObject1 $requestContent,
        iterable $security = ['petstore_auth' => ['write:pets', 'read:pets', ]],
        string $requestMediaType = 'multipart/form-data',
        string $responseMediaType = 'application/json'
    ): \App\DTO\ApiResponse
    {
        return $this->getSuccessfulContent(...$this->uploadFile($parameters, $requestContent, $security, $requestMediaType, $responseMediaType));
    }
    //endregion
}

