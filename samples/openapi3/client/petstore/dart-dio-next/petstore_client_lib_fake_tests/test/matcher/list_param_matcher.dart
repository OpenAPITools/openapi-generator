import 'package:dio/src/parameter.dart';
import 'package:meta/meta.dart';
import 'package:collection/collection.dart';
import 'package:http_mock_adapter/src/matchers/matcher.dart';

class ListParamMatcher<T> extends Matcher {
  final ListParam<T> expected;

  const ListParamMatcher({@required this.expected});

  @override
  bool matches(dynamic actual) {
    return actual is ListParam<T> &&
        ListEquality<T>().equals(
          actual.value,
          expected.value,
        ) &&
        actual.format == expected.format;
  }
}
