//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'cat.freezed.dart';
part 'cat.g.dart';

// ignore_for_file: unused_import


@freezed
abstract class Cat with _$Cat {
  const factory Cat({
    @JsonKey(
      name: r'className',
      required: true,
      includeIfNull: false,
    )
    required String className,
    @JsonKey(
      name: r'color',
      required: false,
      includeIfNull: false,
    )
  
    @Default('red') String color,
    @JsonKey(
      name: r'declawed',
      required: false,
      includeIfNull: false,
    )
    bool? declawed,
  }) = _Cat;

  factory Cat.fromJson(Map<String, dynamic> json) =>
      _$CatFromJson(json);
}

