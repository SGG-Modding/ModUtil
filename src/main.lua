---@meta _
---@diagnostic disable

---@module 'SGG_Modding-DemonDaemon'
local daemon = rom.mods['SGG_Modding-DemonDaemon']
daemon.auto()

---@module 'SGG_Modding-ENVY'
local envy = rom.mods['SGG_Modding-ENVY']
---@module 'SGG_Modding-ENVY-auto'
envy.auto(); _ENV = private

public.private = private

local ready_early = false
local ready_late = false
local ready_final = false
local waiting_ready_early = {}
local waiting_ready_late = {}
local waiting_ready_final = {}

rom.on_import.post(function(name)
	if name == 'RoomLogic.lua' or name == 'RoomManager.lua' then
		private.trigger_ready_final()
	end
end)

function private.trigger_ready_early()
	ready_early = true
	for i,v in ipairs(waiting_ready_early) do
		v()
	end
	for k in pairs(waiting_ready_early) do
		waiting_ready_early[k] = nil
	end
end

function private.trigger_ready_late()
	ready_late = true
	for i,v in ipairs(waiting_ready_late) do
		v()
	end
	for k in pairs(waiting_ready_late) do
		waiting_ready_late[k] = nil
	end
end

function private.trigger_ready_final()
	ready_final = true
	for i,v in ipairs(waiting_ready_final) do
		v()
	end
	for k in pairs(waiting_ready_final) do
		waiting_ready_final[k] = nil
	end
end

function public.on_ready_early(callback)
	if ready_early then callback() end
	table.insert(waiting_ready_early,callback)
end

function public.on_ready_late(callback)
	if ready_late then callback() end
	table.insert(waiting_ready_late,callback)
end

function public.on_ready_final(callback)
	if ready_final then callback() end
	table.insert(waiting_ready_final,callback)
end
