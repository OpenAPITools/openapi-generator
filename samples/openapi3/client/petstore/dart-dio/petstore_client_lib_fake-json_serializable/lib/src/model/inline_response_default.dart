//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:openapi/src/model/foo.dart';
import 'package:json_annotation/json_annotation.dart';

part 'inline_response_default.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class InlineResponseDefault {
  /// Returns a new [InlineResponseDefault] instance.
  InlineResponseDefault({

     this.string,
  });

  @JsonKey(
    
    name: r'string',
    required: false,
    includeIfNull: false
  )


  final Foo? string;



  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineResponseDefault &&
     other.string == string;

  @override
  int get hashCode =>
    string.hashCode;

  factory InlineResponseDefault.fromJson(Map<String, dynamic> json) => _$InlineResponseDefaultFromJson(json);

  Map<String, dynamic> toJson() => _$InlineResponseDefaultToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

