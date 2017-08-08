-- Pet class
local pet = {}
local pet_mt = {
	__name = "pet";
	__index = pet;
}

local function cast_pet(t)
	return setmetatable(t, pet_mt)
end

local function new_pet(name)
	return cast_pet({
		name = name;
		photoUrls = {};
		tags = {};
	})
end

return {
	cast = cast_pet;
	new = new_pet;
}
