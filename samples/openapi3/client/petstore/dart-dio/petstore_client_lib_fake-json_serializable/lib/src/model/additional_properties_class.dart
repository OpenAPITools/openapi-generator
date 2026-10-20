//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'additional_properties_class.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class AdditionalPropertiesClass {
  /// Returns a new [AdditionalPropertiesClass] instance.
  AdditionalPropertiesClass({

     this.mapProperty,

     this.mapOfMapProperty,

     this.mapOfArrayInteger,
  });

  @JsonKey(
    
    name: r'map_property',
    required: false,
    includeIfNull: false,
  )


  final Map<String, String>? mapProperty;



  @JsonKey(
    
    name: r'map_of_map_property',
    required: false,
    includeIfNull: false,
  )


  final Map<String, Map<String, String>>? mapOfMapProperty;



  @JsonKey(
    
    name: r'map_of_array_integer',
    required: false,
    includeIfNull: false,
  )


  final Map<String, List<int>>? mapOfArrayInteger;





    @override
    bool operator ==(Object other) => identical(this, other) || other is AdditionalPropertiesClass &&
      other.mapProperty == mapProperty &&
      other.mapOfMapProperty == mapOfMapProperty &&
      other.mapOfArrayInteger == mapOfArrayInteger;

    @override
    int get hashCode =>
        mapProperty.hashCode +
        mapOfMapProperty.hashCode +
        mapOfArrayInteger.hashCode;

  factory AdditionalPropertiesClass.fromJson(Map<String, dynamic> json) => _$AdditionalPropertiesClassFromJson(json);

  Map<String, dynamic> toJson() => _$AdditionalPropertiesClassToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

