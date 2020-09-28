//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

class ApiException implements Exception {
  ApiException(this.code, this.message);

  ApiException.withInner(this.code, this.message, this.innerException, this.stackTrace);

  int code = 0;
  String message;
  Exception innerException;
  StackTrace stackTrace;

  String toString() {
    if (message == null) return 'ApiException';

    if (innerException == null) {
      return 'ApiException $code: $message';
    }

    return 'ApiException $code: $message (Inner exception: $innerException)\n\n' +
        stackTrace.toString();
  }
}
