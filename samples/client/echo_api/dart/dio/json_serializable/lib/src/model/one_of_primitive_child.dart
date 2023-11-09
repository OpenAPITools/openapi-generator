//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'one_of_primitive_child.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class OneOfPrimitiveChild {
  /// Returns a new [OneOfPrimitiveChild] instance.
  OneOfPrimitiveChild({

     this.name,
  });

  @JsonKey(
    
    name: r'name',
    required: false,
    includeIfNull: false
  )


  final String? name;



  @override
  bool operator ==(Object other) => identical(this, other) || other is OneOfPrimitiveChild &&
     other.name == name;

  @override
  int get hashCode =>
    name.hashCode;

  factory OneOfPrimitiveChild.fromJson(Map<String, dynamic> json) => _$OneOfPrimitiveChildFromJson(json);

  Map<String, dynamic> toJson() => _$OneOfPrimitiveChildToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

