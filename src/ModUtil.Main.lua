---@meta _
---@diagnostic disable

--[[
    ModUtil Main
    Components of ModUtil that depend on loading after Main.lua
]]

-- Global Interception

--[[
	Intercept global keys which are objects to return themselves
	This way we can use other namespaces for UI etc
--]]

local callableCandidateTypes = ModUtil.Internal.callableCandidateTypes
local setSaveIgnore = ModUtil.Internal.setSaveIgnore

setSaveIgnore( "ModUtil", true )

rawset( _ENV, "GLOBALS", ModUtil.Internal._ENV_ORIGINAL )
setSaveIgnore( "GLOBALS", true )

local function isPath( path )
	return path:find("[.]") 
		and not path:find("[.][.]+")
		and not path:find("^[.]")
		and not path:find("[.]$")
end

local function routeKey( self, key )
	local t = type( key )
	if t == "string" and isPath( key ) then
		return ModUtil.Path.Get( key )
	end
	if callableCandidateTypes[ t ] then
		return key
	end
end

local function extendGlobalEnvironment()
	local meta = getmetatable( _ENV ) or { }
	local mi = meta.__index
	local mit = type(mi)
	if mit == "function" then
		meta.__index = ModUtil.Wrap( meta.__index, function( base, self, key )
			local value = base( self, key )
			if value ~= nil then return value end
			return routeKey( self, key )
		end, ModUtil )
	elseif mit == "table" then
		meta.__index = function( self, key )
			local value = mi[key]
			if value ~= nil then return value end
			return routeKey( self, key )
		end
	else
		meta.__index = routeKey
	end

	setmetatable( _ENV, meta )
end

-- Load Trigger Queue

local funcsToLoad = { }
local loadedOnce = false

local function loadFuncs( triggerArgs )
	loadedOnce = true
	for _, v in pairs( funcsToLoad ) do
		v( triggerArgs )
	end
	funcsToLoad = { }
end
---@diagnostic disable-next-line: undefined-global
if OnAnyLoad then
	---@diagnostic disable-next-line: undefined-global
	OnAnyLoad{ loadFuncs }
end

--[[
	Run the provided function once on the next in-game load.

	triggerFunction - the function to run
--]]
function ModUtil.LoadOnce( triggerFunction )
	table.insert( funcsToLoad, triggerFunction )
end

--[[
	Cancel running the provided function once on the next in-game load.

	triggerFunction - the function to cancel running
--]]
function ModUtil.CancelLoadOnce( triggerFunction )
	for i, v in ipairs( funcsToLoad ) do
		if v == triggerFunction then
			table.remove( funcsToLoad, i )
		end
	end
end

-- Mod-Specific Sane Save Data

local objectData = ModUtil.Internal.objectData
local passByValueTypes = ModUtil.Internal.passByValueTypes

local function modDataProxy( value, level )
	level = ( level or 1 ) + 1
	local t = type( value )
	if passByValueTypes[ t ] or t == "string" then
		return value
	end
	if t == "table" then
		if getmetatable( value ) then
			error( "saved data tables cannot have values with metatables.", level )
		end
		return ModUtil.Entangled.ModData( value )
	end
	error( "saved data tables cannot have values of type "..t..".", level )
end

local function modDataKey( key, level )
	local t = type( key )
	if passByValueTypes[ t ] or t == "string" then
		return key
	end
	error( "saved data tables cannot have keys of type "..t..".", ( level or 1 ) + 1 )
end

local function modDataPlain( obj, key, value, level )
	level = ( level or 1 ) + 1
	if modDataKey( key, level ) ~= nil then
		local t = type( value )
		if passByValueTypes[ t ] or t == "string" then
			obj[ key ] = value
		elseif t == "table" then
			if getmetatable( value ) then
				local state
				state, value = pcall( function( ) return objectData[ value ] end )
				if not state or type( value ) ~= "table" then
					error( "saved data tables cannot have values with metatables.", level )
				end
			end
			for k, v in pairs( value ) do
				modDataPlain( value, k, v, level )
			end
			obj[ key ] = value
		else
			error( "saved data tables cannot have values of type "..t..".", level )
		end
	end
end

ModUtil.Metatables.Entangled.ModData = {
	__index = function( self, key )
		return modDataProxy( objectData[ self ][ key ], 2 )
	end,
	__newindex = function( self, key, value )
		modDataPlain( objectData[ self ], key, value, 2 )
	end,
	__len = function( self )
		return #objectData[ self ]
	end,
	__next = function( self, key )
		local key = next( objectData[ self ], key )
		if modDataKey( key, 2 ) ~= nil then
			return key, modDataProxy( objectData[ self ][ key ], 2 )
		end
	end,
	__inext = function( self, idx )
		local idx = inext( objectData[ self ], idx )
		if modDataKey( idx, 2 ) ~= nil then
			return idx, modDataProxy( objectData[ self ][ idx ], 2 )
		end
	end,
	__pairs = function( self )
		return qrawpairs( self )
	end,
	__ipairs = function( self )
		return qrawipairs( self )
	end
}

function ModUtil.Entangled.ModData( value )
	return ModUtil.Proxy( value, ModUtil.Metatables.Entangled.ModData )
end

local function checkModData( level )
	if not loadedOnce then
		error( "saved data is not yet ready to be accessed, wait for the game to load a save.", (level or 1) + 1 )
	end
end

local function getModData( level )
	ModData = ModData or { }
	setSaveIgnore( "ModData", false )
	return ModData
end

ModUtil.Mod.Data = setmetatable( { }, {
	__call = function( self, mod )
		if not loadedOnce then
			return ModUtil.ReferTable( function( )
				checkModData( 3 )
				return self( mod )
			end )
		end
		local modData = getModData( )
		local key = ModUtil.Mods.Inverse[ mod ]
		local data = modData[ key ]
		if not data then
			data = { }
			modData[ key ] = data
		end
		return modDataProxy( data, 2 )
	end,
	__index = function( _, key )
		checkModData( 2 )
		local modData = getModData( )
		return modDataProxy( modData[ key ], 2 )
	end,
	__newindex = function( _, key, value )
		checkModData( 2 )
		local modData = getModData( )
		modDataPlain( modData, key, value, 2 )
	end,
	__len = function( )
		checkModData( 2 )
		local modData = getModData( )
		return #ModData
	end,
	__next = function( _, key )
		checkModData( 2 )
		local modData = getModData( )
		local key = next( modData, key )
		if modDataKey( key, 2 ) ~= nil then
			return key, modDataProxy( modData[ key ], 2 )
		end
	end,
	__inext = function( _, idx )
		checkModData( 2 )
		local modData = getModData( )
		local idx = inext( modData, idx )
		if modDataKey( idx, 2 ) ~= nil then
			return idx, modDataProxy( modData[ idx ], 2 )
		end
	end,
	---@type fun( t ): any, any?, any?
	__pairs = function( self )
		checkModData( 2 )
		return qrawpairs( self )
	end,
	__ipairs = function( self )
		checkModData( 2 )
		return qrawipairs( self )
	end	
} )

local relativeTable = { }

relativeTable[ ModUtil.Mod.Data ] = true

ModUtil.Metatables.Mod = {
	__index = function( self, key )
		local val = ModUtil.Mod[ key ]
		if relativeTable[ val ] then
			return val( self )
		elseif type( val ) == "function" then
			return ModUtil.Wrap( val, function( base, ... )
				return base( self, ... )
			end, ModUtil )
		end
		return val
	end
}

-- Internal Access

do
	local ups = ModUtil.UpValues( function( )
		return _ENV, funcsToLoad, loadFuncs, isPath, routeKey, callableCandidateTypes, setSaveIgnore,
			objectData, passByValueTypes, modDataKey, modDataProxy, modDataPlain, relativeTable, extendGlobalEnvironment
	end )
	ModUtil.Entangled.Union.Add( ModUtil.Internal, ups )
end

extendGlobalEnvironment()