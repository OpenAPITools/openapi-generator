package io.swagger.client;

import java.util.List;
import io.swagger.client.model.*;

public class Responses {
  
  
    public static interface UserResponse {
        public void onResponse(User user);
    }
    public static interface UserListResponse {
        public void onResponse(List<User> userList);
    }
    
  
  
    public static interface CategoryResponse {
        public void onResponse(Category category);
    }
    public static interface CategoryListResponse {
        public void onResponse(List<Category> categoryList);
    }
    
  
  
    public static interface PetResponse {
        public void onResponse(Pet pet);
    }
    public static interface PetListResponse {
        public void onResponse(List<Pet> petList);
    }
    
  
  
    public static interface TagResponse {
        public void onResponse(Tag tag);
    }
    public static interface TagListResponse {
        public void onResponse(List<Tag> tagList);
    }
    
  
  
    public static interface OrderResponse {
        public void onResponse(Order order);
    }
    public static interface OrderListResponse {
        public void onResponse(List<Order> orderList);
    }
    
  
    public static interface StringResponse {
        public void onResponse(String response);
    }

    public static interface StringListResponse {
        public void onResponse(List<String> stringList);
    }
}