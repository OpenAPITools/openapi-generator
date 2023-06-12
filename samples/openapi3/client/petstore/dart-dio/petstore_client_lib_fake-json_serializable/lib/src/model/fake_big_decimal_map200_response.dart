//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'fake_big_decimal_map200_response.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FakeBigDecimalMap200Response {
  /// Returns a new [FakeBigDecimalMap200Response] instance.
  FakeBigDecimalMap200Response({

     this.someId,

     this.someMap,
  });

  @JsonKey(
    
    name: r'someId',
    required: false,
    includeIfNull: false
  )


  final num? someId;



  @JsonKey(
    
    name: r'someMap',
    required: false,
    includeIfNull: false
  )


  final Map<String, num>? someMap;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FakeBigDecimalMap200Response &&
     other.someId == someId &&
     other.someMap == someMap;

  @override
  int get hashCode =>
    someId.hashCode +
    someMap.hashCode;

  factory FakeBigDecimalMap200Response.fromJson(Map<String, dynamic> json) => _$FakeBigDecimalMap200ResponseFromJson(json);

  Map<String, dynamic> toJson() => _$FakeBigDecimalMap200ResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

