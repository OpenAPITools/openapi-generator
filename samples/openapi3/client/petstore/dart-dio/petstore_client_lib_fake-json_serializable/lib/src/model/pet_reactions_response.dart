//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'pet_reactions_response.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class PetReactionsResponse {
  /// Returns a new [PetReactionsResponse] instance.
  PetReactionsResponse({

     this.myReacts,

     this.reactionCounts,
  });

  @JsonKey(
    
    name: r'myReacts',
    required: false,
    includeIfNull: false,
  )


  final Map<String, Map<String, bool>>? myReacts;



  @JsonKey(
    
    name: r'reactionCounts',
    required: false,
    includeIfNull: false,
  )


  final Map<String, Map<String, int>>? reactionCounts;





    @override
    bool operator ==(Object other) => identical(this, other) || other is PetReactionsResponse &&
      other.myReacts == myReacts &&
      other.reactionCounts == reactionCounts;

    @override
    int get hashCode =>
        myReacts.hashCode +
        reactionCounts.hashCode;

  factory PetReactionsResponse.fromJson(Map<String, dynamic> json) => _$PetReactionsResponseFromJson(json);

  Map<String, dynamic> toJson() => _$PetReactionsResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

