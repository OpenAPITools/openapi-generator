//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;


class UserApi {
  UserApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Create user with HTTP info returned
  ///
  /// This can only be done by the logged in user.
  Future createUserWithHttpInfo(User body) async {
    Object postBody = body;

    // verify required params are set
    if (body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/user".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "POST",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Create user
  ///
  ///User body  (required):
  ///     Created user object
  /// This can only be done by the logged in user.
  Future createUser(User body) async {
    final response = await createUserWithHttpInfo(body);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
    }
    return;
  }

  /// Creates list of users with given input array with HTTP info returned
  ///
  /// 
  Future createUsersWithArrayInputWithHttpInfo(List<User> body) async {
    Object postBody = body;

    // verify required params are set
    if (body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/user/createWithArray".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "POST",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Creates list of users with given input array
  ///
  ///List<User> body  (required):
  ///     List of user object
  /// 
  Future createUsersWithArrayInput(List<User> body) async {
    final response = await createUsersWithArrayInputWithHttpInfo(body);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
    }
    return;
  }

  /// Creates list of users with given input array with HTTP info returned
  ///
  /// 
  Future createUsersWithListInputWithHttpInfo(List<User> body) async {
    Object postBody = body;

    // verify required params are set
    if (body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/user/createWithList".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "POST",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Creates list of users with given input array
  ///
  ///List<User> body  (required):
  ///     List of user object
  /// 
  Future createUsersWithListInput(List<User> body) async {
    final response = await createUsersWithListInputWithHttpInfo(body);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
    }
    return;
  }

  /// Delete user with HTTP info returned
  ///
  /// This can only be done by the logged in user.
  Future deleteUserWithHttpInfo(String username) async {
    Object postBody;

    // verify required params are set
    if (username == null) {
     throw ApiException(400, "Missing required param: username");
    }

    // create path and map variables
    String path = "/user/{username}".replaceAll("{format}","json").replaceAll("{" + "username" + "}", username.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "DELETE",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Delete user
  ///
  ///String username  (required):
  ///     The name that needs to be deleted
  /// This can only be done by the logged in user.
  Future deleteUser(String username) async {
    final response = await deleteUserWithHttpInfo(username);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
    }
    return;
  }

  /// Get user by user name with HTTP info returned
  ///
  /// 
  Future<Response> getUserByNameWithHttpInfo(String username) async {
    Object postBody;

    // verify required params are set
    if (username == null) {
     throw ApiException(400, "Missing required param: username");
    }

    // create path and map variables
    String path = "/user/{username}".replaceAll("{format}","json").replaceAll("{" + "username" + "}", username.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "GET",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Get user by user name
  ///
  ///String username  (required):
  ///     The name that needs to be fetched. Use user1 for testing.
  /// 
  Future<User> getUserByName(String username) async {
    final response = await getUserByNameWithHttpInfo(username);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), "User") as User;
    }
    return null;
  }

  /// Logs user into the system with HTTP info returned
  ///
  /// 
  Future<Response> loginUserWithHttpInfo(String username, String password) async {
    Object postBody;

    // verify required params are set
    if (username == null) {
     throw ApiException(400, "Missing required param: username");
    }
    if (password == null) {
     throw ApiException(400, "Missing required param: password");
    }

    // create path and map variables
    String path = "/user/login".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};
      queryParams.addAll(_convertParametersForCollectionFormat("", "username", username));
      queryParams.addAll(_convertParametersForCollectionFormat("", "password", password));

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "GET",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Logs user into the system
  ///
  ///String username  (required):
  ///     The user name for login
  ///String password  (required):
  ///     The password for login in clear text
  /// 
  Future<String> loginUser(String username, String password) async {
    final response = await loginUserWithHttpInfo(username, password);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
      return apiClient.deserialize(_decodeBodyBytes(response), "String") as String;
    }
    return null;
  }

  /// Logs out current logged in user session with HTTP info returned
  ///
  /// 
  Future logoutUserWithHttpInfo() async {
    Object postBody;

    // verify required params are set

    // create path and map variables
    String path = "/user/logout".replaceAll("{format}","json");

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "GET",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Logs out current logged in user session
  ///
  /// 
  Future logoutUser() async {
    final response = await logoutUserWithHttpInfo();
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
    }
    return;
  }

  /// Updated user with HTTP info returned
  ///
  /// This can only be done by the logged in user.
  Future updateUserWithHttpInfo(String username, User body) async {
    Object postBody = body;

    // verify required params are set
    if (username == null) {
     throw ApiException(400, "Missing required param: username");
    }
    if (body == null) {
     throw ApiException(400, "Missing required param: body");
    }

    // create path and map variables
    String path = "/user/{username}".replaceAll("{format}","json").replaceAll("{" + "username" + "}", username.toString());

    // query params
    List<QueryParam> queryParams = [];
    Map<String, String> headerParams = {};
    Map<String, String> formParams = {};

    List<String> contentTypes = [];

    String nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    List<String> authNames = [];

    if (nullableContentType != null && nullableContentType.startsWith("multipart/form-data")) {
      bool hasFields = false;
      MultipartRequest mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    } else {
    }

    return await apiClient.invokeAPI(
      path,
      "PUT",
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Updated user
  ///
  ///String username  (required):
  ///     name that need to be deleted
  ///User body  (required):
  ///     Updated user object
  /// This can only be done by the logged in user.
  Future updateUser(String username, User body) async {
    final response = await updateUserWithHttpInfo(username, body);
    if (response.statusCode >= 400) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    if (response.body != null) {
    }
    return;
  }
}
