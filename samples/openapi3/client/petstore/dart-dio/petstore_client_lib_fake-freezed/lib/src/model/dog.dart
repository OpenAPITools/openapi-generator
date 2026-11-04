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
      name: r'breed',
      required: false,
      includeIfNull: false,
    )
    String? breed,
  }) = _Dog;

  factory Dog.fromJson(Map<String, dynamic> json) =>
      _$DogFromJson(json);
}

