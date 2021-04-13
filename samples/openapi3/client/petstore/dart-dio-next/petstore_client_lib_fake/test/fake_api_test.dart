import 'package:test/test.dart';
import 'package:openapi/openapi.dart';


/// tests for FakeApi
void main() {
  final instance = Openapi().getFakeApi();

  group(FakeApi, () {
    // Health check endpoint
    //
    //Future<HealthCheckResult> fakeHealthGet() async
    test('test fakeHealthGet', () async {
      // TODO
    });

    // test http signature authentication
    //
    //Future fakeHttpSignatureTest(Pet pet, { String query1, String header1 }) async
    test('test fakeHttpSignatureTest', () async {
      // TODO
    });

    // Test serialization of outer boolean types
    //
    //Future<bool> fakeOuterBooleanSerialize({ bool body }) async
    test('test fakeOuterBooleanSerialize', () async {
      // TODO
    });

    // Test serialization of object with outer number type
    //
    //Future<OuterComposite> fakeOuterCompositeSerialize({ OuterComposite outerComposite }) async
    test('test fakeOuterCompositeSerialize', () async {
      // TODO
    });

    // Test serialization of outer number types
    //
    //Future<num> fakeOuterNumberSerialize({ num body }) async
    test('test fakeOuterNumberSerialize', () async {
      // TODO
    });

    // Test serialization of outer string types
    //
    //Future<String> fakeOuterStringSerialize({ String body }) async
    test('test fakeOuterStringSerialize', () async {
      // TODO
    });

    // Test serialization of enum (int) properties with examples
    //
    //Future<OuterObjectWithEnumProperty> fakePropertyEnumIntegerSerialize(OuterObjectWithEnumProperty outerObjectWithEnumProperty) async
    test('test fakePropertyEnumIntegerSerialize', () async {
      // TODO
    });

    // For this test, the body for this request much reference a schema named `File`.
    //
    //Future testBodyWithFileSchema(FileSchemaTestClass fileSchemaTestClass) async
    test('test testBodyWithFileSchema', () async {
      // TODO
    });

    //Future testBodyWithQueryParams(String query, User user) async
    test('test testBodyWithQueryParams', () async {
      // TODO
    });

    // To test \"client\" model
    //
    // To test \"client\" model
    //
    //Future<ModelClient> testClientModel(ModelClient modelClient) async
    test('test testClientModel', () async {
      // TODO
    });

    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    //
    // Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    //
    //Future testEndpointParameters(num number, double double_, String patternWithoutDelimiter, String byte, { int integer, int int32, int int64, double float, String string, Uint8List binary, Date date, DateTime dateTime, String password, String callback }) async
    test('test testEndpointParameters', () async {
      // TODO
    });

    // To test enum parameters
    //
    // To test enum parameters
    //
    //Future testEnumParameters({ BuiltList<String> enumHeaderStringArray, String enumHeaderString, BuiltList<String> enumQueryStringArray, String enumQueryString, int enumQueryInteger, double enumQueryDouble, BuiltList<String> enumFormStringArray, String enumFormString }) async
    test('test testEnumParameters', () async {
      // TODO
    });

    // Fake endpoint to test group parameters (optional)
    //
    // Fake endpoint to test group parameters (optional)
    //
    //Future testGroupParameters(int requiredStringGroup, bool requiredBooleanGroup, int requiredInt64Group, { int stringGroup, bool booleanGroup, int int64Group }) async
    test('test testGroupParameters', () async {
      // TODO
    });

    // test inline additionalProperties
    //
    //Future testInlineAdditionalProperties(BuiltMap<String, String> requestBody) async
    test('test testInlineAdditionalProperties', () async {
      // TODO
    });

    // test json serialization of form data
    //
    //Future testJsonFormData(String param, String param2) async
    test('test testJsonFormData', () async {
      // TODO
    });

    // To test the collection format in query parameters
    //
    //Future testQueryParameterCollectionFormat(BuiltList<String> pipe, BuiltList<String> ioutil, BuiltList<String> http, BuiltList<String> url, BuiltList<String> context) async
    test('test testQueryParameterCollectionFormat', () async {
      // TODO
    });

  });
}
