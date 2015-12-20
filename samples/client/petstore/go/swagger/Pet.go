package swagger

import (
)

type Pet struct {
    id  int64  `json:"id,omitempty"`
    category  Category  `json:"category,omitempty"`
    name  string  `json:"name,omitempty"`
    photoUrls  []string  `json:"photoUrls,omitempty"`
    tags  []Tag  `json:"tags,omitempty"`
    status  string  `json:"status,omitempty"`
    
}
