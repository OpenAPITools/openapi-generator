<?php
/**
 * APIClient.php
 */


/* Autoload the model definition files */
/**
 *
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
	 * @param string $apiKey your API key
	 * @param string $apiServer the address of the API server
	 */
	function __construct($apiKey, $apiServer) {
		$this->apiKey = $apiKey;
		$this->apiServer = $apiServer;
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
				if ($key == 'api_key') {
				    $added_api_key = True;
				}
			}
		}
		if (! $added_api_key) {
		    $headers[] = "api_key: " . $this->apiKey;
		}

		if (is_object($postData) or is_array($postData)) {
			$postData = json_encode($this->sanitizeForSerialization($postData));
		}

		$url = $this->apiServer . $resourcePath;

		$curl = curl_init();
		curl_setopt($curl, CURLOPT_TIMEOUT, 5);
		// return the result on success, rather than just TRUE
		curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
		curl_setopt($curl, CURLOPT_HTTPHEADER, $headers);

		if (! empty($queryParams)) {
			$url = ($url . '?' . http_build_query($queryParams));
		}

		if ($method == self::$POST) {
			curl_setopt($curl, CURLOPT_POST, true);
			curl_setopt($curl, CURLOPT_POSTFIELDS, $postData);
		} else if ($method == self::$PUT) {
			$json_data = json_encode($postData);
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
			throw new Exception("TIMEOUT: api call to " . $url .
				" took more than 5s to return" );
		} else if ($response_info['http_code'] == 200) {
			$data = json_decode($response);
		} else if ($response_info['http_code'] == 401) {
			throw new Exception("Unauthorized API request to " . $url .
					": ".json_decode($response)->message );
		} else if ($response_info['http_code'] == 404) {
			$data = null;
		} else {
			throw new Exception("Can't connect to the api: " . $url .
				" response code: " .
				$response_info['http_code']);
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
  		return rawurlencode($value);
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
            return $object;
        }
	}

	/**
	 * Just pass through the header value for now. Placeholder in case we
	 * find out we need to do something with header values.
	 * @param string $value a string which will be part of the header
	 * @return string the header string
	 */
	public static function toHeaderValue($value) {
  		return $value;
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
    } else if (substr($class, 0, 6) == 'array[') {
      $subClass = substr($class, 6, -1);
      $values = array();
      foreach ($data as $value) {
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


