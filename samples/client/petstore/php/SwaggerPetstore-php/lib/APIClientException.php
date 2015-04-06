<?php
/**
 *  Copyright 2015 Reverb Technologies, Inc.
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

namespace SwaggerPetstore;

use \Exception;

class APIClientException extends Exception {
  protected $response, $response_info;

  public function __construct($message="", $code=0, $response_info=null, $response=null) {
    parent::__construct($message, $code);
    $this->response_info = $response_info;
    $this->response = $response;
  }

  public function getResponse() {
    return $this->response;
  }

  public function getResponseInfo() {
    return $this->response_info;
  }
}
