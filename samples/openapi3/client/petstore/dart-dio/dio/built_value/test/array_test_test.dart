import 'package:test/test.dart';
import 'package:openapi/openapi.dart';

// tests for ArrayTest
void main() {
  final instance = ArrayTestBuilder();
  // add properties to the builder and call build()

  group(ArrayTest, () {
    // BuiltList<String> arrayOfString
    test('to test the property `arrayOfString`', () async {});

    // BuiltList<BuiltList<int>> arrayArrayOfInteger
    test('to test the property `arrayArrayOfInteger`', () async {});

    // BuiltList<BuiltList<ReadOnlyFirst>> arrayArrayOfModel
    test('to test the property `arrayArrayOfModel`', () async {});
  });
}
