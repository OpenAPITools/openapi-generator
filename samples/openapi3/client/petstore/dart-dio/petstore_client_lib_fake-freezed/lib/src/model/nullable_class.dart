//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'nullable_class.freezed.dart';
part 'nullable_class.g.dart';


@freezed
abstract class NullableClass with _$NullableClass {
  const factory NullableClass({
    @JsonKey(
      name: r'integer_prop',
      includeIfNull: false,
    )
    int? integerProp,
    @JsonKey(
      name: r'number_prop',
      includeIfNull: false,
    )
    num? numberProp,
    @JsonKey(
      name: r'boolean_prop',
      includeIfNull: false,
    )
    bool? booleanProp,
    @JsonKey(
      name: r'string_prop',
      includeIfNull: false,
    )
    String? stringProp,
    @JsonKey(
      name: r'date_prop',
      includeIfNull: false,
    )
    DateTime? dateProp,
    @JsonKey(
      name: r'datetime_prop',
      includeIfNull: false,
    )
    DateTime? datetimeProp,
    @JsonKey(
      name: r'array_nullable_prop',
      includeIfNull: false,
    )
    List<Object>? arrayNullableProp,
    @JsonKey(
      name: r'array_and_items_nullable_prop',
      includeIfNull: false,
    )
    List<Object?>? arrayAndItemsNullableProp,
    @JsonKey(
      name: r'array_items_nullable',
      includeIfNull: false,
    )
    List<Object?>? arrayItemsNullable,
    @JsonKey(
      name: r'object_nullable_prop',
      includeIfNull: false,
    )
    Map<String, Object>? objectNullableProp,
    @JsonKey(
      name: r'object_and_items_nullable_prop',
      includeIfNull: false,
    )
    Map<String, Object?>? objectAndItemsNullableProp,
    @JsonKey(
      name: r'object_items_nullable',
      includeIfNull: false,
    )
    Map<String, Object?>? objectItemsNullable,
  }) = _NullableClass;

  factory NullableClass.fromJson(Map<String, dynamic> json) =>
      _$NullableClassFromJson(json);
}

