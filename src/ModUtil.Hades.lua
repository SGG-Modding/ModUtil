---@meta _
---@diagnostic disable

ModUtil.Mod.Register( "Hades", ModUtil )

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
	end
	local screen = printStack
	local components = screen.Components
	
	if first then 
	
		screen.KeepOpen = true
		screen.TextStack = {}
		screen.CullPrintStack = false
		screen.MaxStacks = ModUtil.Config.Hades.PrintStackCapacity
		screen.StackHeight = ModUtil.Config.Hades.PrintStackHeight
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
	for _,s in ipairs( ModUtil.String.Chunk( text, linespan, ModUtil.Config.Hades.PrintStackCapacity ) ) do
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

for k in pairs( _G ) do
	if isTrigger( k ) then
		proxyTrigger( k )
	end
end

-- Internal Access

do
	local ups = ModUtil.UpValues( function( )
		return printStack, orderPrintStack, closePrintStack, printDisplay,
			triggers, isTrigger, proxyTrigger, proxyTriggerMeta, proxyTriggerCallback
	end )
	ModUtil.Entangled.Union.Add( ModUtil.Internal, ups )
end