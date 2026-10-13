//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'dog.freezed.dart';
part 'dog.g.dart';

// ignore_for_file: unused_import


@freezed
abstract class Dog with _$Dog {
  const factory Dog({
    @JsonKey(
      name: r'className',
      includeIfNull: false,
    )
    required String className,
    @JsonKey(
      name: r'color',
      includeIfNull: false,
    )
  
    @Default('red') String color,
    @JsonKey(
      name: r'breed',
      includeIfNull: false,
    )
    String? breed,
  }) = _Dog;

  factory Dog.fromJson(Map<String, dynamic> json) =>
      _$DogFromJson(json);
}

