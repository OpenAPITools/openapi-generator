<?php
/**
 * ApiResponse
 */
namespace OpenAPIServer\Model;

use \InvalidArgumentException;

/**
 * ApiResponse
 */
class ApiResponse
{

    /** @var int $code (optional) */
    private $code;

    /** @var string $type (optional) */
    private $type;

    /** @var string $message (optional) */
    private $message;

    /**
     * ApiResponse constructor
     *
     * @param int|null $code (optional)
     * @param string|null $type (optional)
     * @param string|null $message (optional)
     */
    public function __construct(
        $code = null,
        $type = null,
        $message = null
    ) {
        $this->code = $code;
        $this->type = $type;
        $this->message = $message;
    }

    /**
     * Alternative static class constructor
     *
     * @param mixed[]|null $data Associated array of property values initializing the model
     * @throws InvalidArgumentException when $data doesn't contain required constructor arguments
     * @example $apiResponse = ApiResponse::createFromObject([ 'code' => 'foobar' ]);
     *
     * @return ApiResponse
     */
    public static function createFromObject(array $data = null)
    {
        $code = (isset($data['code'])) ? $data['code'] : null;
        $type = (isset($data['type'])) ? $data['type'] : null;
        $message = (isset($data['message'])) ? $data['message'] : null;
        return new ApiResponse(
            $code,
            $type,
            $message
        );
    }
}
