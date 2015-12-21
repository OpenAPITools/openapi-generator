package swagger

import (
)

type Pet struct {
    Id  int64  `json:"id,omitempty"`
    Category  Category  `json:"category,omitempty"`
    Name  string  `json:"name,omitempty"`
    PhotoUrls  []string  `json:"photoUrls,omitempty"`
    Tags  []Tag  `json:"tags,omitempty"`
    Status  string  `json:"status,omitempty"`
    
}
