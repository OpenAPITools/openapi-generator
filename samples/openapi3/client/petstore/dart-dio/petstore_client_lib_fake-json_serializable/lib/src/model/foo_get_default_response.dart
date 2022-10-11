//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/foo.dart';
import 'package:json_annotation/json_annotation.dart';

part 'foo_get_default_response.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FooGetDefaultResponse {
  /// Returns a new [FooGetDefaultResponse] instance.
  FooGetDefaultResponse({

     this.string,
  });

  @JsonKey(
    
    name: r'string',
    required: false,
    includeIfNull: false
  )


  final Foo? string;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FooGetDefaultResponse &&
     other.string == string;

  @override
  int get hashCode =>
    string.hashCode;

  factory FooGetDefaultResponse.fromJson(Map<String, dynamic> json) => _$FooGetDefaultResponseFromJson(json);

  Map<String, dynamic> toJson() => _$FooGetDefaultResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

