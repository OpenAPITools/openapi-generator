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

use \Swagger\Client\ApiClient;

class Configuration {

  /** @var string[] Associate array to store API key(s) */
  public static $apiKey = array();

  /** string[] Associate array to store API prefix (e.g. Bearer) */
  public static $apiKeyPrefix = array();

  /** @var string Username for HTTP basic authentication */
  public static $username = '';

  /** @var string Password for HTTP basic authentication */
  public static $password = '';

  /** @var \Swagger\Client\ApiClient The default instance of ApiClient */
  public static $apiClient;

  /** @var bool Debug switch (default set to false) */
  public static $debug = false;

  /** @var string Debug file location (log to STDOUT by default) */
  public static $debug_file = 'php://output';

  /*
   *  manually initalize  ApiClient
   */
  public static function init() {
    if (self::$apiClient === null)
      self::$apiClient = new ApiClient();
  }

}
