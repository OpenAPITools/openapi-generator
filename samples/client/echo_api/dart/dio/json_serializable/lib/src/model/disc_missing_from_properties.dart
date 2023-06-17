//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'disc_missing_from_properties.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class DiscMissingFromProperties {
  /// Returns a new [DiscMissingFromProperties] instance.
  DiscMissingFromProperties({

     this.length,
  });

  @JsonKey(
    
    name: r'length',
    required: false,
    includeIfNull: false
  )


  final int? length;



  @override
  bool operator ==(Object other) => identical(this, other) || other is DiscMissingFromProperties &&
     other.length == length;

  @override
  int get hashCode =>
    length.hashCode;

  factory DiscMissingFromProperties.fromJson(Map<String, dynamic> json) => _$DiscMissingFromPropertiesFromJson(json);

  Map<String, dynamic> toJson() => _$DiscMissingFromPropertiesToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

