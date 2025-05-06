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

     this.someid,

     this.somemap,
  });

  @JsonKey(
    
    name: r'someId',
    required: false,
    includeIfNull: false,
  )


  final num? someid;



  @JsonKey(
    
    name: r'someMap',
    required: false,
    includeIfNull: false,
  )


  final Map<String, num>? somemap;





    @override
    bool operator ==(Object other) => identical(this, other) || other is FakeBigDecimalMap200Response &&
      other.someid == someid &&
      other.somemap == somemap;

    @override
    int get hashCode =>
        someid.hashCode +
        somemap.hashCode;

  factory FakeBigDecimalMap200Response.fromJson(Map<String, dynamic> json) => _$FakeBigDecimalMap200ResponseFromJson(json);

  Map<String, dynamic> toJson() => _$FakeBigDecimalMap200ResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

