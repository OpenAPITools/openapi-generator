//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/test_enum.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'object_with_enum.freezed.dart';
part 'object_with_enum.g.dart';


@freezed
abstract class ObjectWithEnum with _$ObjectWithEnum {
  const factory ObjectWithEnum({
    @JsonKey(
      name: r'attribute',
      required: false,
      includeIfNull: false,
      unknownEnumValue: TestEnum.unknownDefaultOpenApi,
    )
  
    @Default(TestEnum.empty) TestEnum attribute,
  }) = _ObjectWithEnum;

  factory ObjectWithEnum.fromJson(Map<String, dynamic> json) =>
      _$ObjectWithEnumFromJson(json);
}

