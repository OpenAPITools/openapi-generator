//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'category.freezed.dart';
part 'category.g.dart';


@freezed
abstract class Category with _$Category {
  const factory Category({
    @JsonKey(
      name: r'id',
      required: false,
      includeIfNull: false,
    )
    int? id,
    @JsonKey(
      name: r'name',
      required: true,
      includeIfNull: false,
    )
  
    @Default('default-name') String name,
  }) = _Category;

  factory Category.fromJson(Map<String, dynamic> json) =>
      _$CategoryFromJson(json);
}

