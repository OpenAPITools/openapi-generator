//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'foo_get_default_response.freezed.dart';
part 'foo_get_default_response.g.dart';


@freezed
abstract class FooGetDefaultResponse with _$FooGetDefaultResponse {
  const factory FooGetDefaultResponse({
    @JsonKey(
      name: r'string',
      required: false,
      includeIfNull: false,
    )
    Foo? string,
  }) = _FooGetDefaultResponse;

  factory FooGetDefaultResponse.fromJson(Map<String, dynamic> json) =>
      _$FooGetDefaultResponseFromJson(json);
}

