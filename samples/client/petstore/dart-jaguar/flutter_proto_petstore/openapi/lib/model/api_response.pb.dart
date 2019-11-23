///
//  Generated code. Do not modify.
//  source: api_response.proto
///
// ignore_for_file: non_constant_identifier_names,library_prefixes,unused_import

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart' as $pb;

class ApiResponse extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = new $pb.BuilderInfo('ApiResponse')
    ..a<int>(1, 'code', $pb.PbFieldType.O3)
    ..aOS(2, 'type')
    ..aOS(3, 'message')
    ..hasRequiredFields = false
  ;

  ApiResponse() : super();
  ApiResponse.fromBuffer(List<int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  ApiResponse.fromJson(String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  ApiResponse clone() => new ApiResponse()..mergeFromMessage(this);
  ApiResponse copyWith(void Function(ApiResponse) updates) => super.copyWith((message) => updates(message as ApiResponse));
  $pb.BuilderInfo get info_ => _i;
  static ApiResponse create() => new ApiResponse();
  static $pb.PbList<ApiResponse> createRepeated() => new $pb.PbList<ApiResponse>();
  static ApiResponse getDefault() => _defaultInstance ??= create()..freeze();
  static ApiResponse _defaultInstance;
  static void $checkItem(ApiResponse v) {
    if (v is! ApiResponse) $pb.checkItemFailed(v, _i.qualifiedMessageName);
  }

  int get code => $_get(0, 0);
  set code(int v) { $_setSignedInt32(0, v); }
  bool hasCode() => $_has(0);
  void clearCode() => clearField(1);

  String get type => $_getS(1, '');
  set type(String v) { $_setString(1, v); }
  bool hasType() => $_has(1);
  void clearType() => clearField(2);

  String get message => $_getS(2, '');
  set message(String v) { $_setString(2, v); }
  bool hasMessage() => $_has(2);
  void clearMessage() => clearField(3);
}

