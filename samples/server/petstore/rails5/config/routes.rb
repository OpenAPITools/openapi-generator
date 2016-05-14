Rails.application.routes.draw do

  def add_swagger_route http_method, path, opts = {}
    full_path = path.gsub(/{(.*?)}/, ':\1')
    action =
      (opts[:isRestfulIndex] && :index) ||
      (opts[:isRestfulShow] && :show) ||
      (opts[:isRestfulCreate] && :create) ||
      (opts[:isRestfulUpdate] && :update) ||
      (opts[:isRestfulDestroy] && :destroy) ||
      opts[:nickname]

    match full_path, to: "#{opts.fetch(:classVarName)}##{action}", via: http_method
  end

  add_swagger_route 'POST', '/v2/pet',
                    classVarName: 'pet', nickname: 'add_pet',
                    isRestfulIndex: false,
                    isRestfulCreate: true,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'DELETE', '/v2/pet/{petId}',
                    classVarName: 'pet', nickname: 'delete_pet',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: true
  add_swagger_route 'GET', '/v2/pet/findByStatus',
                    classVarName: 'pet', nickname: 'find_pets_by_status',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'GET', '/v2/pet/findByTags',
                    classVarName: 'pet', nickname: 'find_pets_by_tags',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'GET', '/v2/pet/{petId}',
                    classVarName: 'pet', nickname: 'get_pet_by_id',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: true,
                    isRestfulDestroy: false
  add_swagger_route 'PUT', '/v2/pet',
                    classVarName: 'pet', nickname: 'update_pet',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'POST', '/v2/pet/{petId}',
                    classVarName: 'pet', nickname: 'update_pet_with_form',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'POST', '/v2/pet/{petId}/uploadImage',
                    classVarName: 'pet', nickname: 'upload_file',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'DELETE', '/v2/store/order/{orderId}',
                    classVarName: 'store', nickname: 'delete_order',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'GET', '/v2/store/inventory',
                    classVarName: 'store', nickname: 'get_inventory',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'GET', '/v2/store/order/{orderId}',
                    classVarName: 'store', nickname: 'get_order_by_id',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'POST', '/v2/store/order',
                    classVarName: 'store', nickname: 'place_order',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'POST', '/v2/user',
                    classVarName: 'user', nickname: 'create_user',
                    isRestfulIndex: false,
                    isRestfulCreate: true,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'POST', '/v2/user/createWithArray',
                    classVarName: 'user', nickname: 'create_users_with_array_input',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'POST', '/v2/user/createWithList',
                    classVarName: 'user', nickname: 'create_users_with_list_input',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'DELETE', '/v2/user/{username}',
                    classVarName: 'user', nickname: 'delete_user',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: true
  add_swagger_route 'GET', '/v2/user/{username}',
                    classVarName: 'user', nickname: 'get_user_by_name',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: true,
                    isRestfulDestroy: false
  add_swagger_route 'GET', '/v2/user/login',
                    classVarName: 'user', nickname: 'login_user',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'GET', '/v2/user/logout',
                    classVarName: 'user', nickname: 'logout_user',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: false,
                    isRestfulShow: false,
                    isRestfulDestroy: false
  add_swagger_route 'PUT', '/v2/user/{username}',
                    classVarName: 'user', nickname: 'update_user',
                    isRestfulIndex: false,
                    isRestfulCreate: false,
                    isRestfulUpdate: true,
                    isRestfulShow: false,
                    isRestfulDestroy: false
end
