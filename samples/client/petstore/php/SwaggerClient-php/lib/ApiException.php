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

namespace SwaggerClient;

use \Exception;

class ApiException extends Exception {

  /**
   * The HTTP body of the server response.
   */
  protected $response_body;

  /**
   * The HTTP header of the server response.
   */
  protected $response_headers;

  public function __construct($message="", $code=0, $responseHeaders=null, $responseBody=null) {
    parent::__construct($message, $code);
    $this->response_headers = $responseHeaders;
    $this->response_body = $responseBody;
  }

  /**
   * Get the HTTP response header
   *
   * @return string HTTP response header
   */
  public function getResponseHeaders() {
    return $this->response_headers;
  }

  /**
   * Get the HTTP response body
   *
   * @return string HTTP response body
   */
  public function getResponseBody() {
    return $this->response_body;
  }

}
