//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'apple_variant1.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AppleVariant1 {
  /// Returns a new [AppleVariant1] instance.
  AppleVariant1({

     this.kind,
  });

  @JsonKey(
    
    name: r'kind',
    required: false,
    includeIfNull: false
  )


  final String? kind;



  @override
  bool operator ==(Object other) => identical(this, other) || other is AppleVariant1 &&
     other.kind == kind;

  @override
  int get hashCode =>
    kind.hashCode;

  factory AppleVariant1.fromJson(Map<String, dynamic> json) => _$AppleVariant1FromJson(json);

  Map<String, dynamic> toJson() => _$AppleVariant1ToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

