package swagger

import (
    "time"
)

type Order struct {
    id  int64  `json:"id,omitempty"`
    petId  int64  `json:"petId,omitempty"`
    quantity  int32  `json:"quantity,omitempty"`
    shipDate  time.Time  `json:"shipDate,omitempty"`
    status  string  `json:"status,omitempty"`
    complete  bool  `json:"complete,omitempty"`
    
}
