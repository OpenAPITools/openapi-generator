//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pet_empty_metadata.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class PetEmptyMetadata {
  /// Returns a new [PetEmptyMetadata] instance.
  PetEmptyMetadata({
  });



    @override
    bool operator ==(Object other) => identical(this, other) || other is PetEmptyMetadata &&

    @override
    int get hashCode =>

  factory PetEmptyMetadata.fromJson(Map<String, dynamic> json) => _$PetEmptyMetadataFromJson(json);

  Map<String, dynamic> toJson() => _$PetEmptyMetadataToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

