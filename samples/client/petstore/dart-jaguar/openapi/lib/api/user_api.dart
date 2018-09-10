import 'package:jaguar_retrofit/annotations/annotations.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_serializer/src/repo/repo.dart';
import 'dart:async';

import 'package:openapi/model/user.dart';


part 'user_api.jretro.dart';

@GenApiClient()
class UserApi extends _$UserApiClient implements ApiClient {
    final Route base;
    final SerializerRepo serializers;

    UserApi({this.base, this.serializers});

    /// Create user
    ///
    /// This can only be done by the logged in user.
    @PostReq(path: "/user")
    Future<void> createUser(
        
        @AsJson() User user
    );

    /// Creates list of users with given input array
    ///
    /// 
    @PostReq(path: "/user/createWithArray")
    Future<void> createUsersWithArrayInput(
        
        @AsJson() List<User> user
    );

    /// Creates list of users with given input array
    ///
    /// 
    @PostReq(path: "/user/createWithList")
    Future<void> createUsersWithListInput(
        
        @AsJson() List<User> user
    );

    /// Delete user
    ///
    /// This can only be done by the logged in user.
    @DeleteReq(path: "/user/:username")
    Future<void> deleteUser(
            @PathParam("username") String username
    );

    /// Get user by user name
    ///
    /// 
    @GetReq(path: "/user/:username")
    Future<User> getUserByName(
            @PathParam("username") String username
    );

    /// Logs user into the system
    ///
    /// 
    @GetReq(path: "/user/login")
    Future<String> loginUser(
        
        @QueryParam("username") String username, 
        
        @QueryParam("password") String password
    );

    /// Logs out current logged in user session
    ///
    /// 
    @GetReq(path: "/user/logout")
    Future<void> logoutUser(
    );

    /// Updated user
    ///
    /// This can only be done by the logged in user.
    @PutReq(path: "/user/:username")
    Future<void> updateUser(
            @PathParam("username") String username
        ,
        @AsJson() User user
    );


}
