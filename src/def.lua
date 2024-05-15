---@meta SGG_Modding-ModUtil

--[[
	DOCUMENTATION FOR MODUTIL IS A WORK IN PROGRESS!
--]]
local modutil = {}

---@type table?
modutil.globals = rom.game

---@param callback fun() runs once modutil's early components are ready (e.g. `globals` for context wraps)
function modutil.on_ready_early(callback) end

---@param callback fun() runs once modutil's late components are ready (e.g. triggers)
function modutil.on_ready_late(callback) end

return modutil