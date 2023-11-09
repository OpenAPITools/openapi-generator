//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'deprecated_object.g.dart';


@Deprecated('DeprecatedObject has been deprecated')
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class DeprecatedObject {
  /// Returns a new [DeprecatedObject] instance.
  DeprecatedObject({

     this.name,
  });

  @JsonKey(
    
    name: r'name',
    required: false,
    includeIfNull: false
  )


  final String? name;



  @override
  bool operator ==(Object other) => identical(this, other) || other is DeprecatedObject &&
     other.name == name;

  @override
  int get hashCode =>
    name.hashCode;

  factory DeprecatedObject.fromJson(Map<String, dynamic> json) => _$DeprecatedObjectFromJson(json);

  Map<String, dynamic> toJson() => _$DeprecatedObjectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

