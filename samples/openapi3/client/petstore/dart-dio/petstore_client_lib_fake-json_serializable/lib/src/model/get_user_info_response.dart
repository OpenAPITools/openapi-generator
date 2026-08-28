//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'get_user_info_response.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class GetUserInfoResponse {
  /// Returns a new [GetUserInfoResponse] instance.
  GetUserInfoResponse({

     this.email,

     this.scopes,
  });

      /// Email address of user, if available
  @JsonKey(
    
    name: r'email',
    required: false,
    includeIfNull: false,
  )


  final String? email;



      /// Auth-Scopes of the user, if available
  @JsonKey(
    
    name: r'scopes',
    required: false,
    includeIfNull: false,
  unknownEnumValue: GetUserInfoResponseScopesEnum.unknownDefaultOpenApi,
  )


  final List<GetUserInfoResponseScopesEnum>? scopes;





    @override
    bool operator ==(Object other) => identical(this, other) || other is GetUserInfoResponse &&
      other.email == email &&
      other.scopes == scopes;

    @override
    int get hashCode =>
        email.hashCode +
        scopes.hashCode;

  factory GetUserInfoResponse.fromJson(Map<String, dynamic> json) => _$GetUserInfoResponseFromJson(json);

  Map<String, dynamic> toJson() => _$GetUserInfoResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

enum GetUserInfoResponseScopesEnum {
@JsonValue(r'app')
app(r'app'),
@JsonValue(r'cms')
cms(r'cms'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const GetUserInfoResponseScopesEnum(this.value);

final String value;

@override
String toString() => value;
}


