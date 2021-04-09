package handlers

type Container struct {
}

func NewContainer() (Container, error) {
    c := Container{}
    return c, nil
}