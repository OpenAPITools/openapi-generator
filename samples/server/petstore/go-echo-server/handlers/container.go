package handlers

// Container will hold all dependencies for your application.
type Container struct {
}

// NewContainer returns an empty or an initialized container for your handlers.
func NewContainer() (Container, error) {
    c := Container{}
    return c, nil
}