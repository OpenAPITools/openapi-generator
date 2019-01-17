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
    final Duration timeout;

    UserApi({this.base, this.serializers, this.timeout = const Duration(minutes: 2)});

    /// Create user
    ///
    /// This can only be done by the logged in user.
    @PostReq(path: "/user")
    Future<void> createUser(
            
              @AsJson() User user
        ) {
        return super.createUser(

        
        user
        ).timeout(timeout);
    }

    /// Creates list of users with given input array
    ///
    /// 
    @PostReq(path: "/user/createWithArray")
    Future<void> createUsersWithArrayInput(
            
              @AsJson() List<User> user
        ) {
        return super.createUsersWithArrayInput(

        
        user
        ).timeout(timeout);
    }

    /// Creates list of users with given input array
    ///
    /// 
    @PostReq(path: "/user/createWithList")
    Future<void> createUsersWithListInput(
            
              @AsJson() List<User> user
        ) {
        return super.createUsersWithListInput(

        
        user
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
              @AsJson() User user
        ) {
        return super.updateUser(
        username

        ,
        user
        ).timeout(timeout);
    }


}
