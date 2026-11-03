//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/single_ref_type.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'all_of_with_single_ref.freezed.dart';
part 'all_of_with_single_ref.g.dart';


@freezed
abstract class AllOfWithSingleRef with _$AllOfWithSingleRef {
  const factory AllOfWithSingleRef({
    @JsonKey(
      name: r'username',
      required: false,
      includeIfNull: false,
    )
    String? username,
    @JsonKey(
      name: r'SingleRefType',
      required: false,
      includeIfNull: false,
      unknownEnumValue: SingleRefType.unknownDefaultOpenApi,
    )
    SingleRefType? singleRefType,
  }) = _AllOfWithSingleRef;

  factory AllOfWithSingleRef.fromJson(Map<String, dynamic> json) =>
      _$AllOfWithSingleRefFromJson(json);
}

