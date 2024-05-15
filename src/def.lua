---@meta SGG_Modding-ModUtil

--[[
	DOCUMENTATION FOR MODUTIL IS A WORK IN PROGRESS!
--]]
local modutil = {}

---@type table?
modutil.globals = rom.game

---@module 'SGG_Modding-ModUtil-Mod'
modutil.mod = ...

---@param callback fun() runs once modutil's early components are ready (e.g. `globals` for context wraps)
function modutil.on_ready_early(callback) end

---@param callback fun() runs once modutil's late components are ready (e.g. triggers)
function modutil.on_ready_late(callback) end

---@param callback fun() runs once the game has finalised (loaded all of its scripts)
function modutil.on_ready_final(callback) end

return modutil