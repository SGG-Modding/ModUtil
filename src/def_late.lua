---@meta SGG_Modding-ModUtil-Late

--[[
	DOCUMENTATION FOR MODUTIL IS A WORK IN PROGRESS!
--]]
_ENV = rom.game

--[==[

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
		setSaveIgnore( "ModData", false )
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
	---@type fun( t ): any, any?, any?
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
---@diagnostic disable-next-line: undefined-global
if OnAnyLoad then
	---@diagnostic disable-next-line: undefined-global
	OnAnyLoad{ function( triggerArgs ) loadFuncs( triggerArgs ) end }
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

-- Internal Access

do
	local ups = ModUtil.UpValues( function( )
		return _ENV, funcsToLoad, loadFuncs, isPath, routeKey, callableCandidateTypes, setSaveIgnore,
			objectData, passByValueTypes, modDataKey, modDataProxy, modDataPlain, relativeTable, extendGlobalEnvironment
	end )
	ModUtil.Entangled.Union.Add( ModUtil.Internal, ups )
end

extendGlobalEnvironment()

-- Debug Printing

local printDisplay = nil

function ModUtil.Hades.PrintDisplay( text , delay, color )
	if type(text) ~= "string" then
		text = tostring(text)
	end
	text = " "..text.." "
	if color == nil then
		color = Color.Yellow
	end
	if delay == nil then
		delay = 5
	end
	if printDisplay then
		Destroy({Ids = {printDisplay.Id}})
	end
	printDisplay = CreateScreenComponent({Name = "BlankObstacle", Group = "PrintDisplay", X = ScreenCenterX, Y = 40 })
	CreateTextBox({ Id = printDisplay.Id, Text = text, FontSize = 22, Color = color, Font = "UbuntuMonoBold"})
	
	if delay > 0 then
		thread(function()
			wait(delay)
			Destroy({Ids = {printDisplay.Id}})
			printDisplay = nil
		end)
	end
end

local printOverhead = { }

function ModUtil.Hades.PrintOverhead(text, delay, color, dest)
	if type(text) ~= "string" then
		text = tostring(text)
	end
	text = " "..text.." "
	if dest == nil then
		dest = CurrentRun.Hero.ObjectId
	end
	if color == nil then
		color = Color.Yellow
	end
	if delay == nil then
		delay = 5
	end
	Destroy({Ids = {printOverhead[dest]}})
	local id = SpawnObstacle({ Name = "BlankObstacle", Group = "PrintOverhead", DestinationId = dest })
	printOverhead[dest] = id
	Attach({ Id = id, DestinationId = dest })
	CreateTextBox({ Id = id, Text = text, FontSize = 32, OffsetX = 0, OffsetY = -150, Color = color, Font = "AlegreyaSansSCBold", Justification = "Center" })
	if delay > 0 then
		thread(function()
			wait(delay)
			if printOverhead[dest] then
				Destroy({Ids = {id}})
				printOverhead[dest] = nil
			end
		end)
	end
end

local printStack = nil

local function closePrintStack()
	if printStack then
		printStack.CullEnabled = false
		PlaySound({ Name = "/SFX/Menu Sounds/GeneralWhooshMENU" })
		printStack.KeepOpen = false
		
		CloseScreen(GetAllIds(printStack.Components),0)
		closeFuncs["PrintStack"] = nil
		printStack = nil
	end
end

local function orderPrintStack(screen,components)
	local v
	if screen.CullPrintStack then
		v = screen.TextStack[1]
		if v.obj then
			Destroy({Ids = {v.obj.Id}})
			components["TextStack_" .. v.tid] = nil
			v.obj = nil
			screen.TextStack[v.tid]=nil
		end
		thread( function()
			local v = screen.TextStack[2]
			if v then
				wait(v.data.Delay)
				if v.obj then
					screen.CullPrintStack = true
				end
			end
		end)
	else
		thread( function()
			local v = screen.TextStack[1]
			if v then 
				wait(v.data.Delay)
				if v.obj then
					screen.CullPrintStack = true
				end
			end
		end)
	end
	screen.CullPrintStack = false
	
	for k,v in pairs(screen.TextStack) do
		components["TextStack_" .. k] = nil
		Destroy({Ids = {v.obj.Id}})
	end
	
	screen.TextStack = CollapseTable(screen.TextStack)
	for i,v in pairs(screen.TextStack) do
		v.tid = i
	end
	if #screen.TextStack == 0 then
		return closePrintStack()
	end
	
	local Ymul = screen.StackHeight+1
	local Ygap = 30
	local Yoff = 26*screen.StackHeight+22
	local n =#screen.TextStack
	
	if n then
		for k=1,math.min(n,Ymul) do
			v = screen.TextStack[k]
			if v then
				local data = v.data
				screen.TextStack[k].obj = CreateScreenComponent({ Name = "rectangle01", Group = "PrintStack", X = -1000, Y = -1000})
				local textStack = screen.TextStack[k].obj
				components["TextStack_" .. k] = textStack
				SetScaleX({Id = textStack.Id, Fraction = 10/6})
				SetScaleY({Id = textStack.Id, Fraction = 0.1})
				SetColor({ Id = textStack.Id, Color = data.Bgcol })
				CreateTextBox({ Id = textStack.Id, Text = data.Text, FontSize = data.FontSize, OffsetX = 0, OffsetY = 0, Color = data.Color, Font = data.Font, Justification = "Center" })
				Attach({ Id = textStack.Id, DestinationId = components.Background.Id, OffsetX = 220, OffsetY = -Yoff })
				Yoff = Yoff - Ygap
			end
		end
	end
	
end

function ModUtil.Hades.PrintStack( text, delay, color, bgcol, fontsize, font, sound )		
	if color == nil then color = {1,1,1,1} end
	if bgcol == nil then bgcol = {0.590, 0.555, 0.657,0.125} end
	if fontsize == nil then fontsize = 13 end
	if font == nil then font = "UbuntuMonoBold" end
	if sound == nil then sound = "/Leftovers/SFX/AuraOff" end
	if delay == nil then delay = 3 end
	
	if type(text) ~= "string" then 
		text = tostring(text)
	end
	text = " "..text.." "
	
	local first = false
	if not printStack then
		first = true
		printStack = { Components = {} }
		closeFuncs["PrintStack"] = closePrintStack
	end
	local screen = printStack
	local components = screen.Components
	
	if first then 
	
		screen.KeepOpen = true
		screen.TextStack = {}
		screen.CullPrintStack = false
		screen.MaxStacks = ModUtil.Hades.PrintStackCapacity
		screen.StackHeight = ModUtil.Hades.PrintStackHeight
		PlaySound({ Name = "/SFX/Menu Sounds/DialoguePanelOutMenu" })
		components.Background = CreateScreenComponent({ Name = "BlankObstacle", Group = "PrintStack", X = ScreenCenterX, Y = 2*ScreenCenterY})
		components.Backing = CreateScreenComponent({ Name = "TraitTray_Center", Group = "PrintStack"})
		Attach({ Id = components.Backing.Id, DestinationId = components.Background.Id, OffsetX = -180, OffsetY = 0 })
		SetColor({ Id = components.Backing.Id, Color = {0.590, 0.555, 0.657, 0.8} })
		SetScaleX({Id = components.Backing.Id, Fraction = 6.25})
		SetScaleY({Id = components.Backing.Id, Fraction = 6/55*(2+screen.StackHeight)})
		
		thread( function()
			while screen do
				wait(0.5)
				if screen.CullEnabled then
					if screen.CullPrintStack then
						orderPrintStack(screen,components)
					end
				end
			end
		end)
		
	end

	if #screen.TextStack >= screen.MaxStacks then return end
	
	screen.CullEnabled = false
	
	local newText = {}
	newText.obj = CreateScreenComponent({ Name = "rectangle01", Group = "PrintStack"})
	newText.data = {Delay = delay, Text = text, Color = color, Bgcol = bgcol, Font = font, FontSize = fontsize}
	SetColor({ Id = newText.obj.Id, Color = {0,0,0,0}})
	table.insert(screen.TextStack, newText)
	
	PlaySound({ Name = sound })
	
	orderPrintStack(screen,components)
	
	screen.CullEnabled = true
	
end

function ModUtil.Hades.PrintStackChunks( text, linespan, ... )
	if not linespan then linespan = 90 end
	for _,s in ipairs( ModUtil.String.Chunk( text, linespan, ModUtil.Hades.PrintStackCapacity ) ) do
		ModUtil.Hades.PrintStack( s, ... )
	end
end

-- Trigger Proxy

local triggers = { }

local function isTrigger( name )
	if name:sub( 1, 2 ) ~= "On" then return false end
	local func = _G[ name ]
	return type( func ) == "function" and debug.getinfo( func, "S" ).what == "C"
end

local proxyTriggerMeta = {
	__index = function( s, k )
		if type( k ) == "string" then
			local w, n = pcall( tonumber, k )
			if w then k = n end
		end
		return rawget( s, k )
	end,
	__newindex = function( s, k, v )
		if type( k ) == "string" then
			local w, n = pcall( tonumber, k )
			if w then k = n end
		end
		rawset( s, k, v )
	end
}

local function proxyTriggerCallback( indexArray, func, args )
	local t = ModUtil.IndexArray.Get( triggers, indexArray ) or setmetatable( { }, proxyTriggerMeta )
	local n = #t + 1
	local f = ModUtil.Override( func, function( ... )
		return t[ n ].Call( ... )
	end )
	table.insert( t, { Args = args, Call = func } )
	ModUtil.IndexArray.Set( triggers, indexArray, t )
	return f
end

local function proxyTrigger( name )
	ModUtil.IndexArray.Wrap( _G, { name }, function( base, args, ... )
		local cargs = ModUtil.Table.Copy( args )
		local func = table.remove( cargs )
		local file = debug.getinfo( 2, "S" ).source
		args[ #args ] = proxyTriggerCallback( { name, file }, func, cargs )
		return base( args, ... )
	end )
end

setmetatable( triggers, {
	__newindex = function( s, k, v )
		if v == true then
			proxyTrigger( k )
			v = s[ k ] or { }
		end
		rawset( s, k, v )
	end
} )
ModUtil.Hades.Triggers = triggers
ModUtil.Identifiers.Inverse[ triggers ] = "ModUtil.Hades.Triggers"

--]==]

return ModUtil