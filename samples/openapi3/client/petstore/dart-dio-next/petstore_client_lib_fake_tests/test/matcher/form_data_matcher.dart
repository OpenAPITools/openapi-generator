import 'package:dio/dio.dart';
import 'package:meta/meta.dart';
import 'package:collection/collection.dart';
import 'package:http_mock_adapter/src/matchers/matcher.dart';

class FormDataMatcher extends Matcher {
  final FormData expected;

  const FormDataMatcher({@required this.expected});

  @override
  bool matches(dynamic actual) {
    if (actual is! FormData) {
      return false;
    }
    final data = actual as FormData;
    return MapEquality<String, String>().equals(
          Map.fromEntries(expected.fields),
          Map.fromEntries(data.fields),
        ) &&
        MapEquality<String, MultipartFile>().equals(
          Map.fromEntries(expected.files),
          Map.fromEntries(data.files),
        );
  }
}
