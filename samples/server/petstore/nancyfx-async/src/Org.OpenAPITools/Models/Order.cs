using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Sharpility.Extensions;
using NodaTime;

namespace Org.OpenAPITools._v2.Models
{
    /// <summary>
    /// An order for a pets from the pet store
    /// </summary>
    public sealed class Order:  IEquatable<Order>
    { 
        /// <summary>
        /// Id
        /// </summary>
        public long? Id { get; private set; }

        /// <summary>
        /// PetId
        /// </summary>
        public long? PetId { get; private set; }

        /// <summary>
        /// Quantity
        /// </summary>
        public int? Quantity { get; private set; }

        /// <summary>
        /// ShipDate
        /// </summary>
        public DateTime? ShipDate { get; private set; }

        /// <summary>
        /// Order Status
        /// </summary>
        public StatusEnum? Status { get; private set; }

        /// <summary>
        /// Complete
        /// </summary>
        public bool? Complete { get; private set; }


        /// <summary>
        /// Empty constructor required by some serializers.
        /// Use Order.Builder() for instance creation instead.
        /// </summary>
        [Obsolete]
        public Order()
        {
        }

        private Order(long? Id, long? PetId, int? Quantity, DateTime? ShipDate, StatusEnum? Status, bool? Complete)
        {
            
            this.Id = Id;
            
            this.PetId = PetId;
            
            this.Quantity = Quantity;
            
            this.ShipDate = ShipDate;
            
            this.Status = Status;
            
            this.Complete = Complete;
            
        }

        /// <summary>
        /// Returns builder of Order.
        /// </summary>
        /// <returns>OrderBuilder</returns>
        public static OrderBuilder Builder()
        {
            return new OrderBuilder();
        }

        /// <summary>
        /// Returns OrderBuilder with properties set.
        /// Use it to change properties.
        /// </summary>
        /// <returns>OrderBuilder</returns>
        public OrderBuilder With()
        {
            return Builder()
                .Id(Id)
                .PetId(PetId)
                .Quantity(Quantity)
                .ShipDate(ShipDate)
                .Status(Status)
                .Complete(Complete);
        }

        public override string ToString()
        {
            return this.PropertiesToString();
        }

        public override bool Equals(object obj)
        {
            return this.EqualsByProperties(obj);
        }

        public bool Equals(Order other)
        {
            return Equals((object) other);
        }

        public override int GetHashCode()
        {
            return this.PropertiesHash();
        }

        /// <summary>
        /// Implementation of == operator for (Order.
        /// </summary>
        /// <param name="left">Compared (Order</param>
        /// <param name="right">Compared (Order</param>
        /// <returns>true if compared items are equals, false otherwise</returns>
        public static bool operator == (Order left, Order right)
        {
            return Equals(left, right);
        }

        /// <summary>
        /// Implementation of != operator for (Order.
        /// </summary>
        /// <param name="left">Compared (Order</param>
        /// <param name="right">Compared (Order</param>
        /// <returns>true if compared items are not equals, false otherwise</returns>
        public static bool operator != (Order left, Order right)
        {
            return !Equals(left, right);
        }

        /// <summary>
        /// Builder of Order.
        /// </summary>
        public sealed class OrderBuilder
        {
            private long? _Id;
            private long? _PetId;
            private int? _Quantity;
            private DateTime? _ShipDate;
            private StatusEnum? _Status;
            private bool? _Complete;

            internal OrderBuilder()
            {
                SetupDefaults();
            }

            private void SetupDefaults()
            {
                _Complete = false;
            }

            /// <summary>
            /// Sets value for Order.Id property.
            /// </summary>
            /// <param name="value">Id</param>
            public OrderBuilder Id(long? value)
            {
                _Id = value;
                return this;
            }

            /// <summary>
            /// Sets value for Order.PetId property.
            /// </summary>
            /// <param name="value">PetId</param>
            public OrderBuilder PetId(long? value)
            {
                _PetId = value;
                return this;
            }

            /// <summary>
            /// Sets value for Order.Quantity property.
            /// </summary>
            /// <param name="value">Quantity</param>
            public OrderBuilder Quantity(int? value)
            {
                _Quantity = value;
                return this;
            }

            /// <summary>
            /// Sets value for Order.ShipDate property.
            /// </summary>
            /// <param name="value">ShipDate</param>
            public OrderBuilder ShipDate(DateTime? value)
            {
                _ShipDate = value;
                return this;
            }

            /// <summary>
            /// Sets value for Order.Status property.
            /// </summary>
            /// <param name="value">Order Status</param>
            public OrderBuilder Status(StatusEnum? value)
            {
                _Status = value;
                return this;
            }

            /// <summary>
            /// Sets value for Order.Complete property.
            /// </summary>
            /// <param name="value">Complete</param>
            public OrderBuilder Complete(bool? value)
            {
                _Complete = value;
                return this;
            }


            /// <summary>
            /// Builds instance of Order.
            /// </summary>
            /// <returns>Order</returns>
            public Order Build()
            {
                Validate();
                return new Order(
                    Id: _Id,
                    PetId: _PetId,
                    Quantity: _Quantity,
                    ShipDate: _ShipDate,
                    Status: _Status,
                    Complete: _Complete
                );
            }

            private void Validate()
            { 
            }
        }

        
        public enum StatusEnum { Placed, Approved, Delivered };
    }
}
