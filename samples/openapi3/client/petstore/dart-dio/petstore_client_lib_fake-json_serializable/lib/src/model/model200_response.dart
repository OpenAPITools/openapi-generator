//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'model200_response.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class Model200Response {
  /// Returns a new [Model200Response] instance.
  Model200Response({

     this.name,

     this.class_,
  });

  @JsonKey(
    
    name: r'name',
    required: false,
    includeIfNull: false
  )


  final int? name;



  @JsonKey(
    
    name: r'class',
    required: false,
    includeIfNull: false
  )


  final String? class_;



  @override
  bool operator ==(Object other) => identical(this, other) || other is Model200Response &&
     other.name == name &&
     other.class_ == class_;

  @override
  int get hashCode =>
    name.hashCode +
    class_.hashCode;

  factory Model200Response.fromJson(Map<String, dynamic> json) => _$Model200ResponseFromJson(json);

  Map<String, dynamic> toJson() => _$Model200ResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

