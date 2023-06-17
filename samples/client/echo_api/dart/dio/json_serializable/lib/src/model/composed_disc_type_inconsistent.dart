//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/disc_type_incorrect.dart';
import 'package:openapi/src/model/fruit_type.dart';
import 'package:json_annotation/json_annotation.dart';

part 'composed_disc_type_inconsistent.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ComposedDiscTypeInconsistent {
  /// Returns a new [ComposedDiscTypeInconsistent] instance.
  ComposedDiscTypeInconsistent({

    required  this.fruitType,
  });

  @JsonKey(
    
    name: r'fruitType',
    required: true,
    includeIfNull: false
  )


  final String fruitType;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ComposedDiscTypeInconsistent &&
     other.fruitType == fruitType;

  @override
  int get hashCode =>
    fruitType.hashCode;

  factory ComposedDiscTypeInconsistent.fromJson(Map<String, dynamic> json) => _$ComposedDiscTypeInconsistentFromJson(json);

  Map<String, dynamic> toJson() => _$ComposedDiscTypeInconsistentToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

