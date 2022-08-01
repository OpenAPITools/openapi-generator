

#include "Pet.h"

using namespace Tiny;

Pet::Pet()
{
	id = long(0);
	category = Category();
	name = std::string();
	photoUrls = std::list<std::string>();
	tags = std::list<Tag>();
	status = std::string();
}

Pet::Pet(std::string jsonString)
{
	this->fromJson(jsonString);
}

Pet::~Pet()
{

}

void
Pet::fromJson(std::string jsonObj)
{
    bourne::json object = bourne::json::parse(jsonObj);

    const char *idKey = "id";

    if(object.has_key(idKey))
    {
        bourne::json value = object[idKey];



        jsonToValue(&id, value, "long");


    }

    const char *categoryKey = "category";

    if(object.has_key(categoryKey))
    {
        bourne::json value = object[categoryKey];




        Category* obj = &category;
		obj->fromJson(value.dump());

    }

    const char *nameKey = "name";

    if(object.has_key(nameKey))
    {
        bourne::json value = object[nameKey];



        jsonToValue(&name, value, "std::string");


    }

    const char *photoUrlsKey = "photoUrls";

    if(object.has_key(photoUrlsKey))
    {
        bourne::json value = object[photoUrlsKey];


        std::list<std::string> photoUrls_list;
        std::string element;
        for(auto& var : value.array_range())
        {

            jsonToValue(&element, var, "std::string");


            photoUrls_list.push_back(element);
        }
        photoUrls = photoUrls_list;


    }

    const char *tagsKey = "tags";

    if(object.has_key(tagsKey))
    {
        bourne::json value = object[tagsKey];


        std::list<Tag> tags_list;
        Tag element;
        for(auto& var : value.array_range())
        {


            element.fromJson(var.dump());

            tags_list.push_back(element);
        }
        tags = tags_list;


    }

    const char *statusKey = "status";

    if(object.has_key(statusKey))
    {
        bourne::json value = object[statusKey];



        jsonToValue(&status, value, "std::string");


    }


}

bourne::json
Pet::toJson()
{
    bourne::json object = bourne::json::object();





    object["id"] = getId();







	object["category"] = getCategory().toJson();





    object["name"] = getName();





    std::list<std::string> photoUrls_list = getPhotoUrls();
    bourne::json photoUrls_arr = bourne::json::array();

    for(auto& var : photoUrls_list)
    {
        photoUrls_arr.append(var);
    }
    object["photoUrls"] = photoUrls_arr;








    std::list<Tag> tags_list = getTags();
    bourne::json tags_arr = bourne::json::array();

    for(auto& var : tags_list)
    {
        Tag obj = var;
        tags_arr.append(obj.toJson());
    }
    object["tags"] = tags_arr;







    object["status"] = getStatus();



    return object;

}

long
Pet::getId()
{
	return id;
}

void
Pet::setId(long  id)
{
	this->id = id;
}

Category
Pet::getCategory()
{
	return category;
}

void
Pet::setCategory(Category  category)
{
	this->category = category;
}

std::string
Pet::getName()
{
	return name;
}

void
Pet::setName(std::string  name)
{
	this->name = name;
}

std::list<std::string>
Pet::getPhotoUrls()
{
	return photoUrls;
}

void
Pet::setPhotoUrls(std::list <std::string> photoUrls)
{
	this->photoUrls = photoUrls;
}

std::list<Tag>
Pet::getTags()
{
	return tags;
}

void
Pet::setTags(std::list <Tag> tags)
{
	this->tags = tags;
}

std::string
Pet::getStatus()
{
	return status;
}

void
Pet::setStatus(std::string  status)
{
	this->status = status;
}



