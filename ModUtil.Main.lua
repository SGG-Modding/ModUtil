--[[
    ModUtil Main
    Components of ModUtil that depend on loading after Main.lua
]]

-- Management

SaveIgnores[ "ModUtil" ] = true

rawset( _ENV, "GLOBALS", ModUtil.Internal._G )
SaveIgnores[ "GLOBALS" ] = true

-- Global Interception

--[[
	Intercept global keys which are objects to return themselves
	This way we can use other namespaces for UI etc
--]]

local callableCandidateTypes = ModUtil.Internal.callableCandidateTypes

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

do 

	local meta = getmetatable( _ENV ) or { }
	if meta.__index then
		meta.__index = ModUtil.Wrap( meta.__index, function( base, self, key )
			local value = base( self, key )
			if value ~= nil then return value end
			return routeKey( self, key )
		end, ModUtil )
	else
		meta.__index = routeKey
	end

	setmetatable( _ENV, meta )

end

--[[
	Create a namespace that can be used for the mod's functions
	and data, and ensure that it doesn't end up in save files.

	modName - the name of the mod
	parent	- the parent mod, or nil if this mod stands alone
--]]
function ModUtil.Mod.Register( first, second, meta )
	local modName, parent
	if type( first ) == "string" then
		modName, parent = first, second
	else
		modName, parent = second, first
	end
	if not parent then
		parent = _G
		SaveIgnores[ modName ] = true
	end
	local mod = parent[ modName ] or { }
	parent[ modName ] = mod
	local path = ModUtil.Identifiers.Data[ parent ]
	if path ~= nil then
		path = path .. '.'
	else
		path = ''
	end
	path = path .. modName
	ModUtil.Mods.Data[ path ] = mod
	ModUtil.Identifiers.Inverse[ path ] = mod
	if meta == false then
		return mod
	end
	return setmetatable( mod, ModUtil.Metatables.Mod )
end

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
			error( "saved data tables cannot have values with metatables", level )
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
					error( "saved data tables cannot have values with metatables", level )
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

ModUtil.Mod.Data = setmetatable( { }, {
	__call = function( _, mod )
		ModData = ModData or { }
		SaveIgnores["ModData"] = false
		local key = ModUtil.Mods.Inverse[ mod ]
		local data = ModData[ key ]
		if not data then
			data = { }
			ModData[ key ] = data
		end
		return modDataProxy( data, 2 )
	end,
	__index = function( _, key )
		ModData = ModData or { }
		return modDataProxy( ModData[ key ], 2 )
	end,
	__newindex = function( _, key, value )
		ModData = ModData or { }
		modDataPlain( ModData, key, value, 2 )
	end,
	__len = function( )
		ModData = ModData or { }
		return #ModData
	end,
	__next = function( _, key )
		ModData = ModData or { }
		local key = next( ModData, key )
		if modDataKey( key, 2 ) ~= nil then
			return key, modDataProxy( ModData[ key ], 2 )
		end
	end,
	__inext = function( _, idx )
		ModData = ModData or { }
		local idx = inext( ModData, idx )
		if modDataKey( idx, 2 ) ~= nil then
			return idx, modDataProxy( ModData[ idx ], 2 )
		end
	end,
	__pairs = function( self )
		return qrawpairs( self )
	end,
	__ipairs = function( self )
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

-- Load Trigger Queue

local funcsToLoad = { }

local function loadFuncs( triggerArgs )
	for _, v in pairs( funcsToLoad ) do
		v( triggerArgs )
	end
	funcsToLoad = { }
end
OnAnyLoad{ function( triggerArgs ) loadFuncs( triggerArgs ) end }


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

-- Internal Access

do
	local ups = ModUtil.UpValues( function( )
		return _G, funcsToLoad, loadFuncs, isPath, routeKey, callableCandidateTypes,
			objectData, passByValueTypes, modDataKey, modDataProxy, modDataPlain, relativeTable
	end )
	ModUtil.Entangled.Union.Add( ModUtil.Internal, ups )
end