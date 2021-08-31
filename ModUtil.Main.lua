--[[
    ModUtil Main
    Components of ModUtil that depend on loading after Main.lua
]]

ModUtil.Anchors = {
	Menu = { },
	CloseFuncs = { }
}

--- bind to locals to minimise environment recursion and improve speed
local ModUtil, pairs, ipairs, table, SaveIgnores, _G
    = ModUtil, pairs, ipairs, table, SaveIgnores, ModUtil.Internal._G

-- Management

SaveIgnores[ "ModUtil" ] = true

rawset( _ENV, "_DEBUG_G", _G )
SaveIgnores[ "_DEBUG_G" ] = true

--[[
	Create a namespace that can be used for the mod's functions
	and data, and ensure that it doesn't end up in save files.

	modName - the name of the mod
	parent	- the parent mod, or nil if this mod stands alone
--]]
function ModUtil.Mod.Register( first, second )
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
	local mod = { }
	parent[ modName ] = mod
	local path = ModUtil.Mods.Inverse[ parent ]
	if path ~= nil then
		path = path .. '.'
	else
		path = ''
	end
	path = path .. modName
	ModUtil.Mods.Data[ path ] = mod
	ModUtil.Identifiers.Inverse[ path ] = mod
	return setmetatable( mod, { __index = ModUtil.Mod } )
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
		return ModUtil.Entangled.ModData( value, level )
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
				local state, value = pcall( function( ) return objectData[ value ] end )
				if not state then
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

--[[
	Tell each screen anchor that they have been forced closed by the game
--]]
local function forceClosed( triggerArgs )
	for _, v in pairs( ModUtil.Anchors.CloseFuncs ) do
		v( nil, nil, triggerArgs )
	end
	ModUtil.Anchors.CloseFuncs = { }
	ModUtil.Anchors.Menu = { }
end
OnAnyLoad{ function( triggerArgs ) forceClosed( triggerArgs ) end }

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
		return _G, forceClosed, funcsToLoad, loadFuncs,
			objectData, passByValueTypes, modDataKey, modDataProxy, modDataPlain
	end )
	ModUtil.Entangled.Union.Add( ModUtil.Internal, ups )
end