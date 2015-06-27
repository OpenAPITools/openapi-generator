<?php
/**
 *  Copyright 2015 SmartBear Software
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

namespace Swagger\Client;

use \Exception;

class ApiException extends Exception {

  /** @var string The HTTP body of the server response. */
  protected $responseBody;

  /** @var string[] The HTTP header of the server response. */
  protected $responseHeaders;

  /**
   * The deserialized response object
   */
  protected $responseObject;

  public function __construct($message="", $code=0, $responseHeaders=null, $responseBody=null) {
    parent::__construct($message, $code);
    $this->responseHeaders = $responseHeaders;
    $this->responseBody = $responseBody;
  }

  /**
   * Get the HTTP response header
   *
   * @return string HTTP response header
   */
  public function getResponseHeaders() {
    return $this->responseHeaders;
  }

  /**
   * Get the HTTP response body
   *
   * @return string HTTP response body
   */
  public function getResponseBody() {
    return $this->responseBody;
  }

  /**
   * sets the deseralized response object (during deserialization)
   * @param mixed $obj
   */
  public function setResponseObject($obj) {
    $this->responseObject = $obj;
  }

  public function getResponseObject() {
    return $this->responseObject;
  }
}
