import 'package:jaguar_retrofit/annotations/annotations.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_mimetype/jaguar_mimetype.dart';
import 'dart:async';

import 'package:openapi/model/user.dart';

part 'user_api.jretro.dart';

@GenApiClient()
class UserApi extends ApiClient with _$UserApiClient {
    final Route base;
    final Map<String, CodecRepo> converters;
    final Duration timeout;

    UserApi({this.base, this.converters, this.timeout = const Duration(minutes: 2)});

    /// Create user
    ///
    /// This can only be done by the logged in user.
    @PostReq(path: "/user")
    Future<void> createUser(
            
             @AsJson() User body
        ) {
        return super.createUser(

        
        body
        ).timeout(timeout);
    }

    /// Creates list of users with given input array
    ///
    /// 
    @PostReq(path: "/user/createWithArray")
    Future<void> createUsersWithArrayInput(
            
             @AsJson() List<User> body
        ) {
        return super.createUsersWithArrayInput(

        
        body
        ).timeout(timeout);
    }

    /// Creates list of users with given input array
    ///
    /// 
    @PostReq(path: "/user/createWithList")
    Future<void> createUsersWithListInput(
            
             @AsJson() List<User> body
        ) {
        return super.createUsersWithListInput(

        
        body
        ).timeout(timeout);
    }

    /// Delete user
    ///
    /// This can only be done by the logged in user.
    @DeleteReq(path: "/user/:username")
    Future<void> deleteUser(
            @PathParam("username") String username
        ) {
        return super.deleteUser(
        username

        ).timeout(timeout);
    }

    /// Get user by user name
    ///
    /// 
    @GetReq(path: "/user/:username")
    Future<User> getUserByName(
            @PathParam("username") String username
        ) {
        return super.getUserByName(
        username

        ).timeout(timeout);
    }

    /// Logs user into the system
    ///
    /// 
    @GetReq(path: "/user/login")
    Future<String> loginUser(
        
            @QueryParam("username") String username, 
        
            @QueryParam("password") String password
        ) {
        return super.loginUser(
        
        username, 
        
        password

        ).timeout(timeout);
    }

    /// Logs out current logged in user session
    ///
    /// 
    @GetReq(path: "/user/logout")
    Future<void> logoutUser(
        ) {
        return super.logoutUser(

        ).timeout(timeout);
    }

    /// Updated user
    ///
    /// This can only be done by the logged in user.
    @PutReq(path: "/user/:username")
    Future<void> updateUser(
            @PathParam("username") String username
            ,
             @AsJson() User body
        ) {
        return super.updateUser(
        username

        ,
        body
        ).timeout(timeout);
    }


}
