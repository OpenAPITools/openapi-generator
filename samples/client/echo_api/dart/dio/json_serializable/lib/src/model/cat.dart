//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:json_annotation/json_annotation.dart';

part 'cat.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Cat {
  /// Returns a new [Cat] instance.
  Cat({

    required  this.className,

     this.color = 'red',

     this.declawed,
  });

  @JsonKey(
    
    name: r'className',
    required: true,
    includeIfNull: false
  )


  final String className;



  @JsonKey(
    defaultValue: 'red',
    name: r'color',
    required: false,
    includeIfNull: false
  )


  final String? color;



  @JsonKey(
    
    name: r'declawed',
    required: false,
    includeIfNull: false
  )


  final bool? declawed;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Cat &&
     other.className == className &&
     other.color == color &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    className.hashCode +
    color.hashCode +
    declawed.hashCode;

  factory Cat.fromJson(Map<String, dynamic> json) => _$CatFromJson(json);

  Map<String, dynamic> toJson() => _$CatToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

