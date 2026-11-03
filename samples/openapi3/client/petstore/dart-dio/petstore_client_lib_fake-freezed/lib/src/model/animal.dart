//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'animal.freezed.dart';
part 'animal.g.dart';


@freezed
abstract class Animal with _$Animal {
  const factory Animal({
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
  }) = _Animal;

  factory Animal.fromJson(Map<String, dynamic> json) =>
      _$AnimalFromJson(json);
}

