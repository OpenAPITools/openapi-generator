package io.swagger.petstore.test;

import io.swagger.client.ApiClient;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.util.Map;

import org.junit.*;

import retrofit.Response;

import static org.junit.Assert.*;

public class StoreApiTest {
    StoreApi api = null;

    @Before
    public void setup() {
        api = new ApiClient().createService(StoreApi.class);
    }

    @Test
    public void testGetInventory() throws Exception {
        api.getInventory().subscribe(new SkeletonSubscriber<Map<String, Integer>>() {
            @Override
            public void onNext(Map<String, Integer> inventory) {
                assertTrue(inventory.keySet().size() > 0);
            }
        });

    }

    @Test
    public void testPlaceOrder() throws Exception {
        final Order order = createOrder();
        api.placeOrder(order).subscribe(SkeletonSubscriber.failTestOnError());
        api.getOrderById(String.valueOf(order.getId())).subscribe(new SkeletonSubscriber<Order>() {
            @Override
            public void onNext(Order fetched) {
                assertEquals(order.getId(), fetched.getId());
                assertEquals(order.getPetId(), fetched.getPetId());
                assertEquals(order.getQuantity(), fetched.getQuantity());
            }
        });
    }

    @Test
    public void testDeleteOrder() throws Exception {
        final Order order = createOrder();
        api.placeOrder(order).subscribe(SkeletonSubscriber.failTestOnError());

        api.getOrderById(String.valueOf(order.getId())).subscribe(new SkeletonSubscriber<Order>() {
            @Override
            public void onNext(Order fetched) {
                assertEquals(fetched.getId(), order.getId());
            }
        });


        api.deleteOrder(String.valueOf(order.getId())).subscribe(SkeletonSubscriber.failTestOnError());
        api.getOrderById(String.valueOf(order.getId()))
                .subscribe(new SkeletonSubscriber<Order>() {
                               @Override
                               public void onNext(Order order) {
                                   throw new RuntimeException("Should not have found deleted order.");
                               }

                               @Override
                               public void onError(Throwable e) {
                                   // should not find deleted order.
                               }
                           }
                );
    }

    private Order createOrder() {
        Order order = new Order();
        order.setId(new Long(System.currentTimeMillis()));
        order.setPetId(new Long(200));
        order.setQuantity(new Integer(13));
        order.setShipDate(new java.util.Date());
        order.setStatus(Order.StatusEnum.PLACED);
        order.setComplete(true);

        return order;
    }
}