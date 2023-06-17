//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_optional_type_incorrect.dart';
import 'package:openapi/src/model/disc_optional_type_correct.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_optional_type_inconsistent.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscOptionalTypeInconsistent {
  /// Returns a new [ComposedDiscOptionalTypeInconsistent] instance.
  ComposedDiscOptionalTypeInconsistent({

     this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: false,
    includeIfNull: false
  )


  final String? fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ComposedDiscOptionalTypeInconsistent &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory ComposedDiscOptionalTypeInconsistent.fromJson(Map<String, dynamic> json) => _$ComposedDiscOptionalTypeInconsistentFromJson(json);

  Map<String, dynamic> toJson() => _$ComposedDiscOptionalTypeInconsistentToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

