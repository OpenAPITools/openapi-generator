//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class UserApi {
  UserApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Executes the same logic as [createUser] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> createUserAsStreamedResponse(User body) async {
    // Verify required params are set.
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = '/user'.replaceAll('{format}', 'json');

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [createUser] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> createUserWithHttpInfo(User body) =>
    apiClient.getResponse(createUserAsStreamedResponse(body));

  /// Create user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [User] body (required):
  ///   Created user object
  Future<void> createUser(User body) async {
    final response = await createUserWithHttpInfo(body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [createUsersWithArrayInput] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> createUsersWithArrayInputAsStreamedResponse(List<User> body) async {
    // Verify required params are set.
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = '/user/createWithArray'.replaceAll('{format}', 'json');

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [createUsersWithArrayInput] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> createUsersWithArrayInputWithHttpInfo(List<User> body) =>
    apiClient.getResponse(createUsersWithArrayInputAsStreamedResponse(body));

  /// Creates list of users with given input array
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [List<User>] body (required):
  ///   List of user object
  Future<void> createUsersWithArrayInput(List<User> body) async {
    final response = await createUsersWithArrayInputWithHttpInfo(body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [createUsersWithListInput] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> createUsersWithListInputAsStreamedResponse(List<User> body) async {
    // Verify required params are set.
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = '/user/createWithList'.replaceAll('{format}', 'json');

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [createUsersWithListInput] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> createUsersWithListInputWithHttpInfo(List<User> body) =>
    apiClient.getResponse(createUsersWithListInputAsStreamedResponse(body));

  /// Creates list of users with given input array
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [List<User>] body (required):
  ///   List of user object
  Future<void> createUsersWithListInput(List<User> body) async {
    final response = await createUsersWithListInputWithHttpInfo(body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [deleteUser] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> deleteUserAsStreamedResponse(String username) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }

    final path = '/user/{username}'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'username' + '}', username.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [deleteUser] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> deleteUserWithHttpInfo(String username) =>
    apiClient.getResponse(deleteUserAsStreamedResponse(username));

  /// Delete user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be deleted
  Future<void> deleteUser(String username) async {
    final response = await deleteUserWithHttpInfo(username);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [getUserByName] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> getUserByNameAsStreamedResponse(String username) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }

    final path = '/user/{username}'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'username' + '}', username.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [getUserByName] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> getUserByNameWithHttpInfo(String username) =>
    apiClient.getResponse(getUserByNameAsStreamedResponse(username));

  /// Get user by user name
  ///
  /// After the method completes, the result is returned as an instance of [User].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The name that needs to be fetched. Use user1 for testing.
  Future<User> getUserByName(String username) async {
    final response = await getUserByNameWithHttpInfo(username);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'User') as User;
    }
    return null;
  }

  /// Executes the same logic as [loginUser] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> loginUserAsStreamedResponse(String username, String password) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }
    if (password == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: password');
    }

    final path = '/user/login'.replaceAll('{format}', 'json');

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('', 'username', username));
      queryParams.addAll(_convertParametersForCollectionFormat('', 'password', password));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [loginUser] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> loginUserWithHttpInfo(String username, String password) =>
    apiClient.getResponse(loginUserAsStreamedResponse(username, password));

  /// Logs user into the system
  ///
  /// After the method completes, the result is returned as an instance of [String].
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   The user name for login
  ///
  /// * [String] password (required):
  ///   The password for login in clear text
  Future<String> loginUser(String username, String password) async {
    final response = await loginUserWithHttpInfo(username, password);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'String') as String;
    }
    return null;
  }

  /// Executes the same logic as [logoutUser] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> logoutUserAsStreamedResponse() async {
    final path = '/user/logout'.replaceAll('{format}', 'json');

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [logoutUser] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> logoutUserWithHttpInfo() =>
    apiClient.getResponse(logoutUserAsStreamedResponse());

  /// Logs out current logged in user session
  ///
  /// Note: This method returns nothing.
  ///
  Future<void> logoutUser() async {
    final response = await logoutUserWithHttpInfo();
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [updateUser] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> updateUserAsStreamedResponse(String username, User body) async {
    // Verify required params are set.
    if (username == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: username');
    }
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = '/user/{username}'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'username' + '}', username.toString());

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>[];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Executes the same logic as [updateUser] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> updateUserWithHttpInfo(String username, User body) =>
    apiClient.getResponse(updateUserAsStreamedResponse(username, body));

  /// Updated user
  ///
  /// This can only be done by the logged in user.
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [String] username (required):
  ///   name that need to be deleted
  ///
  /// * [User] body (required):
  ///   Updated user object
  Future<void> updateUser(String username, User body) async {
    final response = await updateUserWithHttpInfo(username, body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }
}
