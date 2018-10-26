mod api_response;
pub use self::api_response::ApiResponse;
mod category;
pub use self::category::Category;
mod order;
pub use self::order::Order;
mod pet;
pub use self::pet::Pet;
mod tag;
pub use self::tag::Tag;
mod user;
pub use self::user::User;

// TODO(farcaller): sort out files
pub struct File;
