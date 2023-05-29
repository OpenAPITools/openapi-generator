//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/animal.dart';
import 'package:json_annotation/json_annotation.dart';

part 'mixed_properties_and_additional_properties_class.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class MixedPropertiesAndAdditionalPropertiesClass {
  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance.
  MixedPropertiesAndAdditionalPropertiesClass({

     this.uuid,

     this.dateTime,

     this.map,
  });

  @JsonKey(
    
    name: r'uuid',
    required: false,
    includeIfNull: false
  )


  final String? uuid;



  @JsonKey(
    
    name: r'dateTime',
    required: false,
    includeIfNull: false
  )


  final DateTime? dateTime;



  @JsonKey(
    
    name: r'map',
    required: false,
    includeIfNull: false
  )


  final Map<String, Animal>? map;



  @override
  bool operator ==(Object other) => identical(this, other) || other is MixedPropertiesAndAdditionalPropertiesClass &&
     other.uuid == uuid &&
     other.dateTime == dateTime &&
     other.map == map;

  @override
  int get hashCode =>
    uuid.hashCode +
    dateTime.hashCode +
    map.hashCode;

  factory MixedPropertiesAndAdditionalPropertiesClass.fromJson(Map<String, dynamic> json) => _$MixedPropertiesAndAdditionalPropertiesClassFromJson(json);

  Map<String, dynamic> toJson() => _$MixedPropertiesAndAdditionalPropertiesClassToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

