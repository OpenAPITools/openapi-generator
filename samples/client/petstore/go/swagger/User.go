package swagger

import (
)

type User struct {
    Id  int64  `json:"id,omitempty"`
    Username  string  `json:"username,omitempty"`
    FirstName  string  `json:"firstName,omitempty"`
    LastName  string  `json:"lastName,omitempty"`
    Email  string  `json:"email,omitempty"`
    Password  string  `json:"password,omitempty"`
    Phone  string  `json:"phone,omitempty"`
    UserStatus  int32  `json:"userStatus,omitempty"`
    
}
