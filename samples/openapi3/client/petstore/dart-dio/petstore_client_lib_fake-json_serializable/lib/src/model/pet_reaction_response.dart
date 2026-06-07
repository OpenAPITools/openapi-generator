//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pet_reaction_response.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class PetReactionResponse {
  /// Returns a new [PetReactionResponse] instance.
  PetReactionResponse({

     this.petId,

     this.status,
  });

  @JsonKey(
    
    name: r'petId',
    required: false,
    includeIfNull: false,
  )


  final int? petId;



  @JsonKey(
    
    name: r'status',
    required: false,
    includeIfNull: false,
  )


  final String? status;





    @override
    bool operator ==(Object other) => identical(this, other) || other is PetReactionResponse &&
      other.petId == petId &&
      other.status == status;

    @override
    int get hashCode =>
        petId.hashCode +
        status.hashCode;

  factory PetReactionResponse.fromJson(Map<String, dynamic> json) => _$PetReactionResponseFromJson(json);

  Map<String, dynamic> toJson() => _$PetReactionResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

