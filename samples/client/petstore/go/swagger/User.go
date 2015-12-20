package swagger

import (
)

type User struct {
    id  int64  `json:"id,omitempty"`
    username  string  `json:"username,omitempty"`
    firstName  string  `json:"firstName,omitempty"`
    lastName  string  `json:"lastName,omitempty"`
    email  string  `json:"email,omitempty"`
    password  string  `json:"password,omitempty"`
    phone  string  `json:"phone,omitempty"`
    userStatus  int32  `json:"userStatus,omitempty"`
    
}
