import 'dart:convert';

import 'package:collection/collection.dart';
import 'package:http/http.dart';
import 'package:mockito/mockito.dart';

/// A fake client that checks for expected values and returns given responses
///
/// Checks for the expected values (url, headers, body) and throws if not found
///
/// If exception is non-null the request will throw the exception, after other
/// checks are performed
class FakeClient extends Fake implements Client {
  FakeClient({
    this.throwException,
    this.expectedPostRequestBody,
    this.postResponseBody,
    this.expectedGetRequestBody,
    this.getResponseBody,
    this.deleteResponseBody,
    this.expectedPutRequestBody,
    this.putResponseBody,
    this.sendResponseBody,
    this.expectedUrl,
    this.expectedHeaders = const {'Content-Type': 'application/json'},
  });

  Exception throwException;
  Object expectedPostRequestBody;
  String postResponseBody;
  String expectedGetRequestBody;
  String getResponseBody;
  String deleteResponseBody;
  String expectedPutRequestBody;
  String putResponseBody;
  String sendResponseBody;
  String expectedUrl;
  Map<String, String> expectedHeaders;

  @override
  Future<Response> post(url,
      {Map<String, String> headers, body, Encoding encoding}) async {
    // check that the request was made with expected values
    if (url != expectedUrl) {
      throw StateError(
          'POST was called with unexpected url: ${url} should be ${expectedUrl}');
    }
    if (!MapEquality().equals(headers, expectedHeaders)) {
      throw StateError(
          'POST was called with unexpected headers: ${headers} should be ${expectedHeaders}');
    }
    // currently we only expect Map (and subtypes) or Strings
    if (body is Map) {
      if (!MapEquality().equals(body, expectedPostRequestBody)) {
        throw StateError(
            'POST was called with unexpected body: ${body} should be ${expectedPostRequestBody}');
      }
    } else if (body != expectedPostRequestBody) {
      throw StateError(
          'POST was called with unexpected body: ${body} should be ${expectedPostRequestBody}');
    }

    // throw if set to throw
    if (throwException != null) throw throwException;

    return Response(postResponseBody, 200);
  }

  @override
  Future<Response> get(url, {Map<String, String> headers}) async {
    // check that the request was made with expected values
    if (url != expectedUrl) {
      throw StateError(
          'GET was called with unexpected url: ${url} should be ${expectedUrl}');
    }
    if (!MapEquality().equals(headers, expectedHeaders)) {
      throw StateError(
          'GET was called with unexpected headers: ${headers} should be ${expectedHeaders}');
    }

    // throw if set to throw
    if (throwException != null) throw throwException;

    return Response(getResponseBody, 200);
  }

  @override
  Future<Response> delete(url, {Map<String, String> headers}) async {
    // check that the request was made with expected values
    if (url != expectedUrl) {
      throw StateError(
          'DELETE was called with unexpected url: ${url} should be ${expectedUrl}');
    }
    if (!MapEquality().equals(headers, expectedHeaders)) {
      throw StateError(
          'DELETE was called with unexpected headers: ${headers} should be ${expectedHeaders}');
    }

    // throw if set to throw
    if (throwException != null) throw throwException;

    return Response(deleteResponseBody, 200);
  }

  @override
  Future<Response> put(url,
      {Map<String, String> headers, body, Encoding encoding}) async {
    // check that the request was made with expected values
    if (url != expectedUrl) {
      throw StateError(
          'PUT was called with unexpected url: ${url} should be ${expectedUrl}');
    }
    if (!MapEquality().equals(headers, expectedHeaders)) {
      throw StateError(
          'PUT was called with unexpected headers: ${headers} should be ${expectedHeaders}');
    }
    if (body != expectedPutRequestBody) {
      throw StateError(
          'PUT was called with unexpected body: ${body} should be ${expectedPutRequestBody}');
    }

    // throw if set to throw
    if (throwException != null) throw throwException;

    return Response(putResponseBody, 200);
  }

  @override
  Future<StreamedResponse> send(BaseRequest request) async {
    List<int> bytes = utf8.encode(sendResponseBody);
    return StreamedResponse(Stream.fromIterable([bytes]), 200);
  }
}
