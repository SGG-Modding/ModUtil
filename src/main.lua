---@meta _
---@diagnostic disable

---@module 'SGG_Modding-DemonDaemon'
local daemon = rom.mods['SGG_Modding-DemonDaemon']
daemon.auto()

---@module 'SGG_Modding-ENVY'
local envy = rom.mods['SGG_Modding-ENVY']
---@module 'SGG_Modding-ENVY-auto'
envy.auto(); _ENV = private

local function deprecate(name, update, orig)
	orig = orig or public[name]
	public[name] = function(...)
		local env = getfenv(2)
		if env._PLUGIN then
			rom.log.warning(env._PLUGIN.guid .. ' is using ' .. name .. ', which will eventually be removed in future versions.')
			if update then rom.log.warning('To fix this the author should ' .. update) end
		end
		return orig(...)
	end
end

public.private = private

local has_loaded = {}
local awaiting_load = {}
private.trigger_loaded = {}
public.once_loaded = {}

local function define_once_loaded(name)
	has_loaded[name] = false
	awaiting_load[name] = {}
	trigger_loaded[name] = function()
		has_loaded[name] = true
		for _,callback in ipairs(awaiting_load[name]) do
			callback()
		end
		for k in pairs(awaiting_load[name]) do
			awaiting_load[name][k] = nil
		end
	end
	once_loaded[name] = function(callback)
		if has_loaded[name] then return callback() end
		table.insert(awaiting_load[name],callback)
	end 
end

define_once_loaded('mod')
define_once_loaded('game')
define_once_loaded('save')

rom.on_import.post(function(name)
	if name == 'RoomLogic.lua' or name == 'RoomManager.lua' then
		trigger_loaded.game()
		mod.LoadOnce(trigger_loaded.save)
	end
end)

deprecate('on_ready_early', 'use once_loaded.mod instead.', once_loaded.mod)
deprecate('on_ready_late', 'use once_loaded.mod instead.', once_loaded.mod)
deprecate('on_ready_final', 'use once_loaded.game instead.', once_loaded.game)