---@meta SGG_Modding-ModUtil

--[[
	DOCUMENTATION FOR MODUTIL IS A WORK IN PROGRESS!
--]]
local modutil = {}

---@type table?
modutil.globals = rom.game

---@module 'SGG_Modding-ModUtil-Mod'
modutil.mod = ...

modutil.once_loaded = {}

---@param callback fun() runs once modutil's `mod` components are defined
function modutil.once_loaded.mod(callback) end

---@param callback fun() runs once the game has finalised (imported all of its scripts)
function modutil.once_loaded.game(callback) end

---@param callback fun() runs once a save has been loaded (since visiting the main menu)
function modutil.once_loaded.save(callback) end

return modutil