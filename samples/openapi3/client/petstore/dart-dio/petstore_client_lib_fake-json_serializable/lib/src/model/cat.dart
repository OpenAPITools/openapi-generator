//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'cat.g.dart';

// ignore_for_file: unused_import


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Cat extends Animal {
/// Returns a new [Cat] instance.
  Cat({
     this.declawed,
    required  super.className,
     super.color = 'red',
  });

  @JsonKey(
    
    name: r'declawed',
    required: false,
    includeIfNull: false,
  )


  final bool? declawed;





    @override
    bool operator ==(Object other) => identical(this, other) || other is Cat &&
    runtimeType == other.runtimeType &&
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

