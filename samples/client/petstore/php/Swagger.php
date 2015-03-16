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

/* Autoload the model definition files */
/**
 *
 * @param string $className the class to attempt to load
 */
function swagger_autoloader($className) {
  $currentDir = dirname(__FILE__);
  if (file_exists($currentDir . '/' . $className . '.php')) {
    include $currentDir . '/' . $className . '.php';
  } elseif (file_exists($currentDir . '/models/' . $className . '.php')) {
    include $currentDir . '/models/' . $className . '.php';
  }
}
spl_autoload_register('swagger_autoloader');


class APIClient {

  public static $POST = "POST";
  public static $GET = "GET";
  public static $PUT = "PUT";
  public static $DELETE = "DELETE";

  /**
   * @param string $host the address of the API server
   * @param string $headerName a header to pass on requests 
   */
  function __construct($host, $headerName = null, $headerValue = null) {
    $this->host = $host;
    $this->headerName = $headerName;
    $this->headerValue = $headerValue;
  }

  /**
   *  @param integer $seconds Number of seconds before timing out [set to 0 for no timeout]
  */
  public function setTimeout($seconds) {
    if (!is_numeric($seconds)) {
      throw new Exception('Timeout variable must be numeric.');
    }
    $this->curl_timout = $seconds;
  }


  /**
   * @param string $resourcePath path to method endpoint
   * @param string $method method to call
   * @param array $queryParams parameters to be place in query URL
   * @param array $postData parameters to be placed in POST body
   * @param array $headerParams parameters to be place in request header
   * @return mixed
   */
  public function callAPI($resourcePath, $method, $queryParams, $postData,
    $headerParams) {

    $headers = array();

    # Allow API key from $headerParams to override default
    $added_api_key = False;
    if ($headerParams != null) {
      foreach ($headerParams as $key => $val) {
        $headers[] = "$key: $val";
        if ($key == $this->headerName) {
          $added_api_key = True;
        }
      }
    }
    if (! $added_api_key && $this->headerName != null) {
      $headers[] = $this->headerName . ": " . $this->headerValue;
    }

    if (strpos($headers['Content-Type'], "multipart/form-data") < 0 and (is_object($postData) or is_array($postData))) {
      $postData = json_encode($this->sanitizeForSerialization($postData));
    }

    $url = $this->host . $resourcePath;

    $curl = curl_init();
    if ($this->curl_timout) {
      curl_setopt($curl, CURLOPT_TIMEOUT, $this->curl_timout);
    }
    // return the result on success, rather than just TRUE
    curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
    curl_setopt($curl, CURLOPT_HTTPHEADER, $headers);

    if (! empty($queryParams)) {
      $url = ($url . '?' . http_build_query($queryParams));
    }

    if ($method == self::$POST) {
      curl_setopt($curl, CURLOPT_POST, true);
      curl_setopt($curl, CURLOPT_POSTFIELDS, $postData);
    } else if ($method == self::$PATCH) {
      curl_setopt($curl, CURLOPT_CUSTOMREQUEST, "PATCH");
      curl_setopt($curl, CURLOPT_POSTFIELDS, $postData);
    } else if ($method == self::$PUT) {
      curl_setopt($curl, CURLOPT_CUSTOMREQUEST, "PUT");
      curl_setopt($curl, CURLOPT_POSTFIELDS, $postData);
    } else if ($method == self::$DELETE) {
      curl_setopt($curl, CURLOPT_CUSTOMREQUEST, "DELETE");
      curl_setopt($curl, CURLOPT_POSTFIELDS, $postData);
    } else if ($method != self::$GET) {
      throw new Exception('Method ' . $method . ' is not recognized.');
    }
    curl_setopt($curl, CURLOPT_URL, $url);

    // Make the request
    $response = curl_exec($curl);
    $response_info = curl_getinfo($curl);

    // Handle the response
    if ($response_info['http_code'] == 0) {
      throw new APIClientException("TIMEOUT: api call to " . $url .
        " took more than 5s to return", 0, $response_info, $response);
    } else if ($response_info['http_code'] >= 200 && $response_info['http_code'] <= 299 ) {
      $data = json_decode($response);
      if (json_last_error() > 0) { // if response is a string
        $data = $response;
      }
    } else if ($response_info['http_code'] == 401) {
      throw new APIClientException("Unauthorized API request to " . $url .
          ": " . serialize($response), 0, $response_info, $response);
    } else if ($response_info['http_code'] == 404) {
      $data = null;
    } else {
      throw new APIClientException("Can't connect to the api: " . $url .
        " response code: " .
        $response_info['http_code'], 0, $response_info, $response);
    }
    return $data;
  }

  /**
   * Build a JSON POST object
   */
  protected function sanitizeForSerialization($data)
  {
    if (is_scalar($data) || null === $data) {
      $sanitized = $data;
    } else if ($data instanceof \DateTime) {
      $sanitized = $data->format(\DateTime::ISO8601);
    } else if (is_array($data)) {
      foreach ($data as $property => $value) {
        $data[$property] = $this->sanitizeForSerialization($value);
      }
      $sanitized = $data;
    } else if (is_object($data)) {
      $values = array();
      foreach (array_keys($data::$swaggerTypes) as $property) {
        $values[$property] = $this->sanitizeForSerialization($data->$property);
      }
      $sanitized = $values;
    } else {
      $sanitized = (string)$data;
    }

    return $sanitized;
  }

  /**
   * Take value and turn it into a string suitable for inclusion in
   * the path, by url-encoding.
   * @param string $value a string which will be part of the path
   * @return string the serialized object
   */
  public static function toPathValue($value) {
      return rawurlencode(toString($value));
  }

  /**
   * Take value and turn it into a string suitable for inclusion in
   * the query, by imploding comma-separated if it's an object.
   * If it's a string, pass through unchanged. It will be url-encoded
   * later.
   * @param object $object an object to be serialized to a string
   * @return string the serialized object
   */
  public static function toQueryValue($object) {
        if (is_array($object)) {
            return implode(',', $object);
        } else {
            return toString($object);
        }
  }

  /**
   * Take value and turn it into a string suitable for inclusion in
   * the header. If it's a string, pass through unchanged
   * If it's a datetime object, format it in ISO8601
   * @param string $value a string which will be part of the header
   * @return string the header string
   */
  public static function toHeaderValue($value) {
      return toString($value);
  }

  /**
   * Take value and turn it into a string suitable for inclusion in
   * the http body (form parameter). If it's a string, pass through unchanged
   * If it's a datetime object, format it in ISO8601
   * @param string $value the value of the form parameter
   * @return string the form string
   */
  public static function toFormValue($value) {
      return toString($value);
  }

  /**
   * Take value and turn it into a string suitable for inclusion in
   * the parameter. If it's a string, pass through unchanged
   * If it's a datetime object, format it in ISO8601
   * @param string $value the value of the parameter
   * @return string the header string
   */
  public static function toString($value) {
    if ($value instanceof \DateTime) { // datetime in ISO8601 format
      return $value->format(\DateTime::ISO8601);
    }
    else {
      return $value;
    }
   }

  /**
   * Deserialize a JSON string into an object
   *
   * @param object $object object or primitive to be deserialized
   * @param string $class class name is passed as a string
   * @return object an instance of $class
   */

  public static function deserialize($data, $class)
  {
    if (null === $data) {
      $deserialized = null;
    } elseif (substr($class, 0, 4) == 'map[') {
      $inner = substr($class, 4, -1);
      $values = array();
      if(strrpos($inner, ",") !== false) {
        $subClass_array = explode(',', $inner, 2);
        $subClass = $subClass_array[1];
        foreach ($data as $key => $value) {
          $values[] = array($key => self::deserialize($value, $subClass));
        }        
      }
      $deserialized = $values;
    } elseif (strcasecmp(substr($class, 0, 6),'array[') == 0) {
      $subClass = substr($class, 6, -1);
      $values = array();
      foreach ($data as $key => $value) {
        $values[] = self::deserialize($value, $subClass);
      }
      $deserialized = $values;
    } elseif ($class == 'DateTime') {
      $deserialized = new \DateTime($data);
    } elseif (in_array($class, array('string', 'int', 'float', 'bool'))) {
      settype($data, $class);
      $deserialized = $data;
    } else {
      $instance = new $class();
      foreach ($instance::$swaggerTypes as $property => $type) {
        if (isset($data->$property)) {
          $instance->$property = self::deserialize($data->$property, $type);
        }
      }
      $deserialized = $instance;
    }

    return $deserialized;
  }

}

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
