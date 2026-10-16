//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'get_user_info_response.freezed.dart';
part 'get_user_info_response.g.dart';


@freezed
abstract class GetUserInfoResponse with _$GetUserInfoResponse {
  const factory GetUserInfoResponse({
    /// Email address of user, if available
    @JsonKey(
      name: r'email',
      required: false,
      includeIfNull: false,
    )
    String? email,
    /// Auth-Scopes of the user, if available
    @JsonKey(
      name: r'scopes',
      required: false,
      includeIfNull: false,
      unknownEnumValue: GetUserInfoResponseScopesEnum.unknownDefaultOpenApi,
    )
    List<GetUserInfoResponseScopesEnum>? scopes,
  }) = _GetUserInfoResponse;

  factory GetUserInfoResponse.fromJson(Map<String, dynamic> json) =>
      _$GetUserInfoResponseFromJson(json);
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


