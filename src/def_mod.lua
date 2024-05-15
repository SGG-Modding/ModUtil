---@meta SGG_Modding-ModUtil-Mod

---@alias SGG_Modding-ModUtil-Mod*-nil boolean|string|number|integer|function|table|thread|userdata|lightuserdata

--[[
	DOCUMENTATION FOR MODUTIL IS A WORK IN PROGRESS!
]]
local mod = {
	Mod = { },
	Args = { },
	String = { },
	Table = { },
	Path = { },
	Array = { },
	IndexArray = { },
	Entangled = { },
	Metatables = { },
    Hades = { }
}

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*getter fun(iter: table<K,V>, key: K): value: V?

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*setter fun(iter: table<K,V>, key: K, value: V)

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*length fun(iter: table<K,V>): length: integer

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*next fun(iter: table<K,V>, key: K?): key: K?, value: V?

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*inext fun(iter: table<K,V>, index: integer?): index: integer?, value: V?

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*pairs fun(iter: table<K,V>): next: SGG_Modding-ModUtil-Mod*next<K,V>, iter: table<K,V>

---@generic K : SGG_Modding-ModUtil-Mod*-nil
---@generic V : any
---@alias SGG_Modding-ModUtil-Mod*ipairs fun(iter: table<K,V>): inext: SGG_Modding-ModUtil-Mod*inext<K,V>, iter: table<K,V>

-- base next, doesn't invoke __index
---@type SGG_Modding-ModUtil-Mod*next
function rawnext( iter, key ) end

-- next that does invoke __index
---@type SGG_Modding-ModUtil-Mod*next
function next( iter, key ) end

-- truly raw pairs, ignores __next and __pairs
---@type SGG_Modding-ModUtil-Mod*pairs
function rawpairs( iter )
	return rawnext, iter
end

-- quasi-raw pairs, invokes __next but ignores __pairs
---@type SGG_Modding-ModUtil-Mod*pairs
function qrawpairs( iter )
    return next, iter
end

-- doesn't invoke __index just like rawnext
---@type SGG_Modding-ModUtil-Mod*inext
function rawinext( iter, index ) end

-- invokes __inext just like the new next
---@type SGG_Modding-ModUtil-Mod*inext
function inext( iter, index ) end

-- truly raw ipairs, ignores __inext and __ipairs
---@type SGG_Modding-ModUtil-Mod*ipairs
function rawipairs( t ) end

-- quasi-raw ipairs, invokes __inext but ignores __ipairs
---@type SGG_Modding-ModUtil-Mod*ipairs
function qrawipairs( t ) end

-- ignore __tostring (not thread safe?)
---@param obj any
---@return string rep
function rawtostring( obj ) end

---@param fn integer | function?
---@return table?
function getfenv( fn ) end

--[[
	Replace a function's _ENV with a new environment table.
	Global variable lookups (including function calls) in that function
	will use the new environment table rather than the normal one.
	This is useful for function-specific overrides. The new environment
	table should generally have _G as its __index (and usually __newindex),
    so that any globals other than those being deliberately overridden operate as usual.
]]
---@param fn integer | function?
---@param env table?
function setfenv( fn, env ) end

-- base table.insert
---@param list table
---@param value any
function table.rawinsert( list, value ) end

-- base table.insert
---@param list table
---@param pos integer
---@param value any
function table.rawinsert( list, pos, value ) end

-- table.insert that respects metamethods
---@param list table
---@param value any
function table.insert( list, value ) end

-- table.insert that respects metamethods
---@param list table
---@param pos integer
---@param value any
function table.insert( list, pos, value ) end

-- base table.remove
---@param list table
---@param pos integer?
---@return any value
function table.rawremove( list, pos ) end

-- table.remove that respects metamethods
---@param list table
---@param pos integer?
---@return any value
function table.remove( list, pos ) end

-- base table.unpack
---@generic T: any
---@param list T[]
---@param start integer?
---@param stop integer?
---@return ...
function table.rawunpack( list, start, stop ) end

-- table.unpack that respects metamethods
---@generic T: any
---@param list T[]
---@param start integer?
---@param stop integer?
---@return ...
function table.unpack( list, start, stop ) end

-- base table.rawconcat
---@param list table
---@param sep string?
---@param start integer?
---@param stop integer?
---@return string concat
function table.rawconcat( list, sep, start, stop ) end

-- table.concat that respects metamethods and includes more values
---@param list table
---@param sep string?
---@param start integer?
---@param stop integer?
---@return string concat
function table.concat( list, sep, start, stop ) end

---@class Proxy
mod.Metatables.Proxy = {}
---@type SGG_Modding-ModUtil-Mod*getter
function mod.Metatables.Proxy:__index( key ) end
---@type SGG_Modding-ModUtil-Mod*setter
function mod.Metatables.Proxy:__newindex( key, value ) end
---@type SGG_Modding-ModUtil-Mod*length
function mod.Metatables.Proxy:__len() end
---@type SGG_Modding-ModUtil-Mod*next
function mod.Metatables.Proxy:__next( key ) end
---@type SGG_Modding-ModUtil-Mod*inext
function mod.Metatables.Proxy:__inext( index ) end
---@type SGG_Modding-ModUtil-Mod*pairs
function mod.Metatables.Proxy:__pairs( ) end
---@type SGG_Modding-ModUtil-Mod*ipairs
function mod.Metatables.Proxy:__ipairs( ) end

---@generic K: SGG_Modding-ModUtil-Mod*-nil
---@generic V: any
---@param data table<K,V>
---@param meta table?
---@return Proxy<K,V> proxy
function mod.Proxy( data, meta ) end

---@class Raw: Proxy
mod.Metatables.Raw = {}

---@generic K: SGG_Modding-ModUtil-Mod*-nil
---@generic V: any
---@param data table<K,V>
---@return Raw<K,V> proxy
function mod.Raw( data ) end

-- Operations on Callables

---@overload fun(_: any, obj: table): boolean
mod.Callable = { Func = { } }

---@param obj table | function
---@return table? parent
---@return table | function? call
function mod.Callable.Get( obj ) end

---@param obj table | function
---@param call function
---@return table? parent
---@return table | function? call
function mod.Callable.Set( obj, call ) end

---@generic C: table|function
---@param obj C
---@param mcall fun(call:C, ...): C
---@return table? parent
---@return C? call
function mod.Callable.Map( obj, mcall, ... ) end

---@param obj table | function
---@return table | function? call
function mod.Callable.Func.Get( obj ) end

---@param obj table | function
---@param call function
---@return table | function? call
function mod.Callable.Func.Set( obj, call ) end

---@generic C: table|function
---@param obj C
---@param mcall fun(call:C, ...): C
---@return C? call
function mod.Callable.Func.Map( obj, mcall, ... ) end

-- Data Misc

---@generic I: any
---@generic O: any
---@param map fun(input: I): output: O
---@param ... I
---@return O ...
function mod.Args.Map( map, ... ) end

---@generic A: any
---@param n integer
---@param ... A
---@return A ...
function mod.Args.Take( n, ... ) end

--- TODO: ...

--[==[

function mod.Args.Drop( n, ... )
	local args = table.pack( ... )
	return table.rawunpack( args, n + 1, args.n )
end

function mod.Args.Join( args, ... )
	local args = mod.Array.Join( args, table.pack( ... ) )
	return table.rawunpack( args )
end

function mod.Table.Map( tbl, map )
	local out = { }
	for k, v in pairs( tbl ) do
		out[ k ] = map( v )
	end
	return out
end

function mod.Table.Mutate( tbl, map )
	for k, v in pairs( tbl ) do
		tbl[ k ] = map( v )
	end
end

function mod.Table.Replace( target, data )
	for k in pairs( target ) do
		target[ k ] = data[ k ]
	end
	for k, v in pairs( data ) do
		target[ k ] = v
	end
end

function mod.Table.UnKeyed( tbl )
	local n = #tbl
	for k in pairs( tbl ) do
		if type( k ) ~= "number" or k > n or k < 1 or k ~= math.floor(k) then return false end
	end
	return true
end


function mod.String.Join( sep, ... )
	return table.rawconcat( table.pack( ... ), sep )
end

function mod.String.Chunk( text, chunkSize, maxChunks )
	local chunks = { "" }
	local cs = 0
	local ncs = 1
	for chr in text:gmatch( "." ) do
		cs = cs + 1
		if cs > chunkSize or chr == "\n" then
			ncs = ncs + 1
			if maxChunks and ncs > maxChunks then
				return chunks
			end
			chunks[ ncs ] = ""
			cs = 0
		end
		if chr ~= "\n" then
			chunks[ ncs ] = chunks[ ncs ] .. chr
		end
	end
	return chunks
end

-- String Representations

local escapeCharacters = {
	['\\'] = '\\\\', ["'"] =  "\'", ['"'] = '\"',
	['\n'] = '\\n', ['\r'] = '\\r', ['\t'] = '\\t',
	['\a'] = '\\a', ['\b'] = '\\b', ['\f'] = '\\f'
}

local function literalString( str )
	for chr, esc in pairs( escapeCharacters ) do
		str = str:gsub( chr, esc )
	end
	return "'" .. str .. "'"
end

mod.ToString = mod.Callable.Set( { }, function( _, o )
	---@type boolean|string
	local identifier = o ~= nil and mod.Identifiers.Data[ o ]
	identifier = identifier and identifier .. ": " or ""
	return identifier .. mod.ToString.Static( o )
end )

function mod.ToString.Address( o )
	local t = type( o )
	if t == "string" or passByValueTypes[ t ] then return nil end
	return rawtostring( o ):match( ": 0*([0-9A-F]*)" )
end

function mod.ToString.Static( o )
	local t = type( o )
	if t == "string" or passByValueTypes[ t ] then return tostring( o ) end
	return tostring( o ):gsub( ": 0*", ": ", 1 )
end

function mod.ToString.Value( o )
	local t = type( o )
	if t == 'string' then
		return literalString( o )
	end
	if passByValueTypes[ t ] then
		return tostring( o )
	end
	return '<' .. mod.ToString( o ) .. '>'
end

function mod.ToString.Key( o )
	local t = type( o )
	if t == 'string' then
		if not excludedFieldNames[ o ] and o:match( "^[a-zA-Z_][a-zA-Z0-9_]*$" ) then
			return o
		end
		return '[' .. literalString( o ) .. ']'
	end
	if passByValueTypes[ t ] then
		return "[" .. tostring( o ) .. "]"
	end
    return '<' .. mod.ToString( o ) .. '>'
end

function mod.ToString.TableKeys( o )
	if type( o ) == 'table' then
		local out = { }
		for k in pairs( o ) do
			rawinsert( out , mod.ToString.Key( k ) )
		end
		return rawconcat( out, ',' )
	end
end

local function isNamespace( obj )
	return obj == _ENV_ORIGINAL or obj == _ENV_REPLACED or obj == objectData or mod.Mods.Inverse[ obj ]
		or ( getmetatable( obj ) == mod.Metatables.Raw and isNamespace( objectData[ obj ][ "data" ] ) )
end

local function isNotNamespace( obj )
	return not isNamespace( obj )
end

local showTableAddrs = false

local repk, repv = mod.ToString.Key, mod.ToString.Value

local function deepLoop( o, limit, dlimit, indent, seen, cond, depth )
	depth = depth or 0
	if dlimit then
		if dlimit <= depth then
			return limit, repv( o )
		end
	end
	local _indent = ''
	if indent then
		local __indent = { }
		for i = 1, depth, 1 do
			_indent[ i ] = indent
		end
		_indent = table.rawconcat( __indent )
	end
	if type( o ) ~= "table" or (seen and seen[ o ]) or (cond and not cond( o )) then
		return limit, repv( o )
	end
	if seen then seen[ o ] = true end
	local m = getmetatable( o )
	---@type boolean|string
	local h = showTableAddrs or ( m and m.__call ) or isNamespace( o )
	h = ( h and repv( o ) or "" ) .. '{' .. ( indent and '\n' .. _indent .. indent or '' )
	local out = { }
	local i = 0
	local broken = false
	depth = depth + 1
	for j, v in ipairs( o ) do
		if cond and not cond( v ) then break end
		i = j
		if limit and limit <= 0 then
			out[ i ] = "..."
			broken = true
			break
		end
		if limit then
			limit = limit - 1
		end
		limit, v = deepLoop( v, limit, dlimit, indent, seen, cond, depth )
		out[ i ] = v
	end
	if not broken then
		local j = i
		for k, v in pairs( o ) do
			if ( not cond or cond( v ) ) and ( not isInt( k ) or k < 1 or k > j ) then
				i = i + 1
				if limit and limit <= 0 then
					out[ i ] = '...'
					break
				end
				if limit then
					limit = limit - 1
				end
				limit, v = deepLoop( v, limit, dlimit, indent, seen, cond, depth )
				out[ i ] = repk( k ) .. ' = ' .. v
			end
		end
	end
	local _end = ( indent and '\n' .. _indent or '' ) .. '}'
	if i == 0 then return limit, h .. _end end
	out[ 1 ] = h .. out[ 1 ]
	out[ i ] = out[ i ] .. _end
	return limit, rawconcat( out, ',' .. ( indent and '\n' .. _indent .. indent or ' ' ) )
end

function mod.ToString.Shallow( object, limit, indent )
	local _, out = deepLoop( object, limit, 1, indent )
	return out
end

mod.ToString.Deep = mod.Callable.Set( { }, function( _, object, limit, depth, indent )
	local _, out = deepLoop( object, limit, depth, indent, { } )
	return out
end )


function mod.ToString.Deep.NoNamespaces( object, limit, depth, indent )
	local _, out = deepLoop( object, limit, depth, indent, { }, isNotNamespace )
	return out
end

function mod.ToString.Deep.Namespaces( object, limit, depth, indent )
	local _, out = deepLoop( object, limit, depth, indent, { }, isNamespace )
	return out
end

-- Print

mod.Print = mod.Callable.Set( { }, function ( _, ... )
	print( ... )
	---@diagnostic disable-next-line: undefined-global
	if DebugPrint then mod.Print.Debug( ... ) end
	if io then
		if io.stdout ~= io.output( ) then
			mod.Print.ToFile( io.output( ), ... )
			io.flush( )
		end
	end
end )

function mod.Print.ToFile( file, ... )
	local close = false
	if type( file ) == "string" and io then
		file = io.open( file, "a" )
		close = true
	end
	file:write( mod.Args.Map( tostring, ... ) )
	if close then
		file:close( )
	end
end

function mod.Print.Debug( ... )
	local text = mod.String.Join( "\t", mod.Args.Map( tostring, ... ) ):gsub( "\t", "    " )
	for line in text:gmatch( "([^\n]+)" ) do
		---@diagnostic disable-next-line: undefined-global
		DebugPrint{ Text = line }
	end
end

function mod.Print.Traceback( level )
	level = (level or 1) + 1
	mod.Print("Traceback:")
	local cont = true
	while cont do
		local text = debug.traceback( "", level ):sub( 2 )
		local first = true
		local i = 1
		cont = false
		for line in text:gmatch( "([^\n]+)" ) do
			if first then
				first = false
			else
				if line == "\t" then
					break
				end
				if i > 10 then
					cont = true
					break
				end
				mod.Print( line )
				i = i + 1
			end
		end
		level = level + 10
	end
end

function mod.Print.DebugInfo( level )
	level = level or 1
	local text
	text = mod.ToString.Deep( debug.getinfo( level + 1 ) )
	mod.Print( "Debug Info:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
end

function mod.Print.Namespaces( level )
	level = level or 1
	local text
	mod.Print("Namespaces:")
	text = mod.ToString.Deep.Namespaces( mod.Locals( level + 1 ) )
	mod.Print( "\t" .. "Locals:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
	text = mod.ToString.Deep.Namespaces( mod.UpValues( level + 1 ) )
	mod.Print( "\t" .. "UpValues:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
	text = mod.ToString.Deep.Namespaces( _G )
	mod.Print( "\t" .. "Globals:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
end

function mod.Print.Variables( level )
	level = level or 1
	local text
	mod.Print("Variables:")
	text = mod.ToString.Deep.NoNamespaces( mod.Locals( level + 1 ) )
	mod.Print( "\t" .. "Locals:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
	text = mod.ToString.Deep.NoNamespaces( mod.UpValues( level + 1 ) )
	mod.Print( "\t" .. "UpValues:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
	text = mod.ToString.Deep.NoNamespaces( _G )
	mod.Print( "\t" .. "Globals:" .. "\t" .. text:sub( 1 + text:find( ">" ) ) )
end

--[[
	Call a function with the provided arguments
	instead of halting when an error occurs it prints the entire error traceback
--]]
function mod.DebugCall( f, ... )
	return xpcall( f, function( err )
		mod.Print( err )
		mod.Print.DebugInfo( 2 )
		--mod.Print.Namespaces( 2 )
		--mod.Print.Variables( 2 )
		mod.Print.Traceback( 2 )
    end, ... )
end

-- Data Manipulation

--[[
	Return a slice of an array table, python style
		would be written state[ start : stop : step ] in python

	start and stop are offsets rather than ordinals
		meaning 0 corresponds to the start of the array
		and -1 corresponds to the end
--]]
function mod.Array.Slice( state, start, stop, step )
	local slice = { }
	local n = #state
	start = start or 0
	if start < 0 then
		start = start + n
	end
	stop = stop or n
	if stop < 0 then
		stop = stop + n
	end
	for i = start, stop - 1, step do
		table.insert( slice, state[ i + 1 ] )
	end
	return slice
end

function mod.Array.Copy( a )
	return { table.unpack( a ) }
end

--[[
	Concatenates arrays, in order.

	a, ... - the arrays
--]]
function mod.Array.Join( a, ... )
	local b = ...
	if not b then return mod.Array.Copy( a ) end
	local c = { }
	local j = 0
	for i, v in ipairs( a ) do
		c[ i ] = v
		j = i
	end
	for i, v in ipairs( b ) do
		c[ i + j ] = v
	end
	return mod.Array.Join( c, mod.Args.Drop( 1, ... ) )
end

mod.Table.Copy = mod.Callable.Set( { }, function( _, t )
	local c = { }
	for k, v in pairs( t ) do
		c[ k ] = v
	end
	return c
end )

function mod.Table.Copy.Deep( t )
	local c = { }
	for k, v in pairs( t ) do
		if type( v ) == "table" then
			v = mod.Table.Copy( v )
		end
		c[ k ] = v
	end
	return c
end

function mod.Table.Clear( t )
	for k in pairs( t ) do
		t[ k ] = nil
	end
	return t
end

function mod.Table.Transpose( t )
	local i = { }
	for k, v in pairs( t ) do
		i[ v ] = k
	end
	return i
end

function mod.Table.Flip( t )
	local i = mod.Table.Transpose( t )
	mod.Table.Clear( t )
	for k, v in pairs( i ) do
		t[ k ] = v
	end
	return t
end

--[[
	Set all the values in inTable corresponding to keys
	in nilTable to nil.

	For example, if inTable is
	{
		Foo = 5,
		Bar = 6,
		Baz = {
			InnerFoo = 5,
			InnerBar = 6
		}
	}

	and nilTable is
	{
		Foo = true,
		Baz = {
			InnerBar = true
		}
	}

	then the result will be
	{
		Foo = nil
		Bar = 6,
		Baz = {
			InnerFoo = 5,
			InnerBar = nil
		}
	}
--]]
function mod.Table.NilMerge( inTable, nilTable )
	for nilKey, nilVal in pairs( nilTable ) do
		local inVal = inTable[ nilKey ]
		if type( nilVal ) == "table" and type( inVal ) == "table" then
			mod.Table.NilMerge( inVal, nilVal )
		else
			inTable[ nilKey ] = nil
		end
	end
	return inTable
end

--[[
	Set all the the values in inTable corresponding to values
	in setTable to their values in setTable.

	For example, if inTable is
	{
		Foo = 5,
		Bar = 6
	}

	and setTable is
	{
		Foo = 7,
		Baz = {
			InnerBar = 8
		}
	}

	then the result will be
	{
		Foo = 7,
		Bar = 6,
		Baz = {
			InnerBar = 8
		}
	}
--]]
function mod.Table.Merge( inTable, setTable )
	for setKey, setVal in pairs( setTable ) do
		local inVal = inTable[ setKey ]
		if type( setVal ) == "table" and type( inVal ) == "table" then
			mod.Table.Merge( inVal, setVal )
		else
			inTable[ setKey ] = setVal
		end
	end
	return inTable
end

function mod.Table.MergeKeyed( inTable, setTable )
	for setKey, setVal in pairs( setTable ) do
		local inVal = inTable[ setKey ]
		if type( setVal ) == "table" and type( inVal ) == "table" then
			if mod.Table.UnKeyed( setVal ) and mod.Table.UnKeyed( inVal ) then
				mod.Table.Replace( inVal, setVal )
			else
				mod.Table.MergeKeyed( inVal, setVal )
			end
		else
			inTable[ setKey ] = setVal
		end
	end
	return inTable
end

-- Index Array Manipulation

--[[
	Safely retrieve the a value from deep inside a table, given
	an array of indices into the table.

	For example, if indexArray is { "a", 1, "c" }, then
	Table[ "a" ][ 1 ][ "c" ] is returned. If any of Table[ "a" ],
	Table[ "a" ][ 1 ], or Table[ "a" ][ 1 ][ "c" ] are nil, then nil
	is returned instead.

	Table			 - the table to retrieve from
	indexArray	- the list of indices
--]]
function mod.IndexArray.Get( baseTable, indexArray )
	local node = baseTable
	for _, key in ipairs( indexArray ) do
		if type( node ) ~= "table" then
			return nil
		end
		local nodeType = mod.Node.Inverse[ key ]
		if nodeType then
			node = mod.Node.Data[ nodeType ].Get( node )
		else
			node = node[ key ]
		end
	end
	return node
end

--[[
	Safely set a value deep inside a table, given an array of
	indices into the table, and creating any necessary tables
	along the way.

	For example, if indexArray is { "a", 1, "c" }, then
	Table[ "a" ][ 1 ][ "c" ] = Value once this function returns.
	If any of Table[ "a" ] or Table[ "a" ][ 1 ] does not exist, they
	are created.

	baseTable	 - the table to set the value in
	indexArray	- the list of indices
	value	- the value to add
--]]
function mod.IndexArray.Set( baseTable, indexArray, value )
	if next( indexArray ) == nil then
		return false -- can't set the input argument
	end
	local n = #indexArray -- change to shallow copy + table.remove later
	local node = baseTable
	for i = 1, n - 1 do
		local key = indexArray[ i ]
		if not mod.Node.New( node, key ) then return false end
		local nodeType = mod.Node.Inverse[ key ]
		if nodeType then
			node = mod.Node.Data[ nodeType ].Get( node )
		else
			node = node[ key ]
		end
	end
	local key = indexArray[ n ]
	local nodeType = mod.Node.Inverse[ key ]
	if nodeType then
		return mod.Node.Data[ nodeType ].Set( node, value )
	end
	node[ key ] = value
	return true
end

function mod.IndexArray.Map( baseTable, indexArray, map, ... )
	return mod.IndexArray.Set( baseTable, indexArray, map( mod.IndexArray.Get( baseTable, indexArray ), ... ) )
end

-- Path Manipulation

function mod.Path.Join( p, ... )
	local q = ...
	if not q then return p end
	if p == '' then return mod.Path.Join( q, mod.Args.Drop( 1, ... ) ) end
	if q == '' then return mod.Path.Join( p, mod.Args.Drop( 1, ... ) ) end
	return mod.Path.Join( p .. '.' .. q, mod.Args.Drop( 1, ... ) )
end

function mod.Path.Map( path, map, ... )
	return mod.IndexArray.Map( _G, mod.Path.IndexArray( path ), map, ... )
end

--[[
	Create an index array from the provided Path.

	The returned array can be used as an argument to the safe table
	manipulation functions, such as mod.IndexArray.Set and mod.IndexArray.Get.

	path - a dot-separated string that represents a path into a table
--]]
function mod.Path.IndexArray( path )
	if type( path ) == "table" then return path end -- assume index array is given
	local s = ""
	local i = { }
	for c in path:gmatch( "." ) do
		if c ~= "." then
			s = s .. c
		else
			table.insert( i, s )
			s = ""
		end
	end
	if #s > 0 then
		table.insert( i, s )
	end
	return i
end

--[[
	Safely get a value from a Path.

	For example, mod.Path.Get( "a.b.c" ) returns a.b.c.
	If either a or a.b is nil, nil is returned instead.

	path - the path to get the value
	base - (optional) The table to retreive the value from.
				 If not provided, retreive a global.
--]]
function mod.Path.Get( path, base )
	return mod.IndexArray.Get( base or _G, mod.Path.IndexArray( path ) )
end

--[[
	Safely get set a value to a Path.

	For example, mod.Path.Set( "a.b.c", 1 ) sets a.b.c = 1.
	If either a or a.b is nil, they are created.

	path - the path to get the value
	base - (optional) The table to retreive the value from.
				 If not provided, retreive a global.
--]]
function mod.Path.Set( path, value, base )
	return mod.IndexArray.Set( base or _G, mod.Path.IndexArray( path ), value )
end

-- Metaprogramming Shenanigans

---@type table<string,function|boolean?>
local stackLevelProperty
stackLevelProperty = {
	here = function( self )
		local thread = objectData[ self ][ "thread" ]
		local cursize = objectData[ self ][ "level" ] + 1
		while debug.getinfo( thread, cursize, "f" ) do
			cursize = cursize + 1
		end
		return cursize - objectData[ self ][ "size" ] - 1
	end,
	top = function( self )
		local thread = objectData[ self ][ "thread" ]
		local level = objectData[ self ][ "level" ]
		local cursize = level + 1
		while debug.getinfo( thread, cursize, "f" ) do
			cursize = cursize + 1
		end
		return cursize - level - 1
	end,
	there = function( self ) return objectData[ self ][ "level" ] end,
	bottom = function( self ) return objectData[ self ][ "size" ] end,
	co = function( self ) return objectData[ self ][ "thread" ] end,
	func = function( self )
		return debug.getinfo( self.co, self.here, "f" ).func
	end
}

---@type table<string,function|boolean?>
local stackLevelFunction = {
	gethook = function( self, ... )
		return pusherror( debug.gethook, self.co, ... )
	end,
	sethook = function( self, ... )
		return pusherror( debug.sethook, self.co, ... )
	end,
	getlocal = function( self, ... )
		return pusherror( debug.getlocal, self.co, self.here, ... )
	end,
	setlocal = function( self, ... )
		return pusherror( debug.setlocal, self.co, self.here, ... )
	end,
	getinfo = function( self, ... )
		return pusherror( debug.getinfo, self.co, self.here, ... )
	end,
	getupvalue = function( self, ... )
		return pusherror( debug.getupvalue, self.func, ... )
	end,
	setupvalue = function( self, ... )
		return pusherror( debug.setupvalue, self.func, ... )
	end,
	upvalueid = function( self, ... )
		return pusherror( debug.upvalueid, self.func, ... )
	end,
	upvaluejoin = function( self, ... )
		return pusherror( debug.upvaluejoin, self.func, ... )
	end
}

local stackLevelInterface = {}
for k, v in pairs( stackLevelProperty ) do
	stackLevelInterface[ k ] = v
	stackLevelProperty[ k ] = true
end
for k, v in pairs( stackLevelFunction ) do 
	stackLevelInterface[ k ] = v
	stackLevelFunction[ k ] = true
end

mod.Metatables.StackLevel = {
	__index = function( self, key )
		if stackLevelProperty[ key ] then
			return stackLevelInterface[ key ]( self )
		elseif stackLevelFunction[ key ] then
			local func = stackLevelInterface[ key ]
			return function( ... )
				return pusherror( func, self, ... )
			end
		end
	end,
	__newindex = function( ) end,
	__len = function( )
		return 0
	end,
	__next = function( self, key )
		repeat
			key = next( stackLevelInterface, key )
		until stackLevelFunction[ key ] == nil
		return key, self[ key ]
	end,
	__inext = function( ) end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end,
	__ipairs = function( self )
		return function( ) end, self
	end,
	__eq = function( self, other )
		return objectData[ self ][ "thread" ] == objectData[ other ][ "thread" ]
		and objectData[ self ][ "size" ] == objectData[ other ][ "size" ]
		and objectData[ self ][ "level" ] == objectData[ other ][ "level" ]
	end
}

function mod.StackLevel( level )
	level = ( level or 1 )
	local thread = coroutine.running( )
	local size = level + 1
	if not debug.getinfo( thread, level, "f" ) then return end
	while debug.getinfo( thread, size, "f" ) do
		size = size + 1
	end
	size = size - level - 1
	if size > 0 then
		return mod.Proxy( { level = level, size = size, thread = thread }, mod.Metatables.StackLevel )
	end
end

mod.Metatables.StackLevels = {
	__index = function( self, level )
		return mod.StackLevel( ( level or 0 ) + objectData[ self ][ "level" ].here )
	end,
	__newindex = function( ) end,
	__len = function( self )
		return objectData[ self ][ "level" ].bottom
	end,
	__next = function( self, level )
		level = ( level or 0 ) + 1
		local stackLevel = self[ level ]
		if stackLevel then
			return level, stackLevel
		end
	end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end
}
mod.Metatables.StackLevels.__ipairs = mod.Metatables.StackLevels.__pairs
mod.Metatables.StackLevels.__inext = mod.Metatables.StackLevels.__next

function mod.StackLevels( level )
	return mod.Proxy( { level = mod.StackLevel( level or 0 ) }, mod.Metatables.StackLevels )
end


local excludedUpValueNames = toLookup{ "_ENV" }

mod.Metatables.UpValues = {
	__index = function( self, name )
		if excludedUpValueNames[ name ] then return end
		local func = objectData[ self ][ "func" ]
		local idx = 0
		repeat
			idx = idx + 1
			local n, value = debug.getupvalue( func, idx )
			if n == name then
				return value
			end
		until not n
	end,
	__newindex = function( self, name, value )
		if excludedUpValueNames[ name ] then return end
		local func = objectData[ self ][ "func" ]
		local idx = name and 0 or -1
		repeat
			idx = idx + 1
			local n = debug.getupvalue( func, idx )
			if n == name then
				debug.setupvalue( func, idx, value )
				return
			end
		until not n
	end,
	__len = function( )
		return 0
	end,
	__next = function( self, name )
		local func = objectData[ self ][ "func" ]
		local idx = name and 0 or -1
		repeat
			idx = idx + 1
			local n = debug.getupvalue( func, idx )
			if n == name then
				local value
				repeat
					idx = idx + 1
					n, value = debug.getupvalue( func, idx )
					if n and not excludedUpValueNames[ n ] then
						return n, value
					end
				until not n
			end
		until not n
	end,
	__inext = function( ) end,
	__pairs = function( self )
		return qrawpairs( self )
	end,
	__ipairs = function( self )
		return function( ) end, self
	end
}

mod.UpValues = mod.Callable.Set( { }, function( _, func )
	if type( func ) ~= "function" then
		func = debug.getinfo( ( func or 1 ) + 1, "f" ).func
	end
	return mod.Proxy( { func = func }, mod.Metatables.UpValues )
end )

local idData = { }
setmetatable( idData, { __mode = "k" } )

local function getUpValueIdData( id )
	local tbl = idData[ id ]
	return tbl.func, tbl.idx
end

local function setUpValueIdData( id, func, idx )
	local tbl = idData[ id ]
	if not tbl then
		tbl = { }
		idData[ id ] = tbl
	end
	tbl.func, tbl.idx = func, idx
end

local upvaluejoin = debug.upvaluejoin

---@diagnostic disable-next-line: duplicate-set-field
function debug.upvaluejoin( f1, n1, f2, n2 )
	upvaluejoin( f1, n1, f2, n2 )
	setUpValueIdData( debug.upvalueid( f1, n1 ), f2, n2 )
end

mod.Metatables.UpValues.Ids = {
	__index = function( self, idx )
		local func = objectData[ self ][ "func" ]
		local name = debug.getupvalue( func, idx )
		if name and not excludedUpValueNames[ name ] then
			local id = debug.upvalueid( func, idx )
			setUpValueIdData( id, func, idx )
			return id
		end
	end,
	__newindex = function( self, idx, value )
		local func = objectData[ self ][ "func" ]
		local name = debug.getupvalue( func, idx )
		if name and not excludedUpValueNames[ name ] then
			local func2, idx2 = getUpValueIdData( value )
			debug.upvaluejoin( func, idx, func2, idx2 )
			return
		end
	end,
	__len = function( self )
		return debug.getinfo( objectData[ self ][ "func" ], 'u' ).nups
	end,
	__next = function ( self, idx )
		local func = objectData[ self ][ "func" ]
		idx = idx or 0
		local name
		while true do
			idx = idx + 1
			name = debug.getupvalue( func, idx )
			if not name then return end
			if not excludedUpValueNames[ name ] then
				return idx, self[ idx ]
			end
		end
	end,
	__pairs = function( self )
		return qrawpairs( self )
	end
}
mod.Metatables.UpValues.Ids.__inext = mod.Metatables.UpValues.Ids.__next
mod.Metatables.UpValues.Ids.__ipairs = mod.Metatables.UpValues.Ids.__pairs

function mod.UpValues.Ids( func )
	if type(func) ~= "function" then
		func = debug.getinfo( ( func or 1 ) + 1, "f" ).func
	end
	return mod.Proxy( { func = func }, mod.Metatables.UpValues.Ids )
end

mod.Metatables.UpValues.Values = {
	__index = function( self, idx )
		local name, value = debug.getupvalue( objectData[ self ][ "func" ], idx )
		if name and not excludedUpValueNames[ name ] then
			return value
		end
	end,
	__newindex = function( self, idx, value )
		local func = objectData[ self ][ "func" ]
		local name = debug.getupvalue( func, idx )
		if name and not excludedUpValueNames[ name ] then
			debug.setupvalue( func, idx, value )
			return
		end
	end,
	__len = function( self )
		return debug.getinfo( objectData[ self ][ "func" ], 'u' ).nups
	end,
	__next = function ( self, idx )
		local func = objectData[ self ][ "func" ]
		idx = idx or 0
		local name, value
		while true do
			idx = idx + 1
			name, value = debug.getupvalue( func, idx )
			if not name then return end
			if not excludedUpValueNames[ name ] then
				return idx, value
			end
		end
	end,
	__pairs = mod.Metatables.UpValues.Ids.__pairs,
	__ipairs = mod.Metatables.UpValues.Ids.__ipairs
}
mod.Metatables.UpValues.Values.__inext = mod.Metatables.UpValues.Values.__next

function mod.UpValues.Values( func )
	if type(func) ~= "function" then
		func = debug.getinfo( ( func or 1 ) + 1, "f" ).func
	end
	return mod.Proxy( { func = func }, mod.Metatables.UpValues.Values )
end

mod.Metatables.UpValues.Names = {
	__index = function( self, idx )
		local name = debug.getupvalue( objectData[ self ][ "func" ], idx )
		if name and not excludedUpValueNames[ name ] then
			return name
		end
	end,
	__newindex = function( ) end,
	__len = mod.Metatables.UpValues.Values.__len,
	__next = function ( self, idx )
		local func = objectData[ self ][ "func" ]
		idx = idx or 0
		local name
		while true do
			idx = idx + 1
			name = debug.getupvalue( func, idx )
			if not name then return end
			if not excludedUpValueNames[ name ] then
				return idx, name
			end
		end
	end,
	__pairs = mod.Metatables.UpValues.Ids.__pairs,
	__ipairs = mod.Metatables.UpValues.Ids.__ipairs
}
mod.Metatables.UpValues.Names.__inext = mod.Metatables.UpValues.Names.__next

function mod.UpValues.Names( func )
	if type(func) ~= "function" then
		func = debug.getinfo( ( func or 1 ) + 1, "f" ).func
	end
	return mod.Proxy( { func = func }, mod.Metatables.UpValues.Names )
end

mod.Metatables.UpValues.Stacked = {
	__index = function( self, name )
		if excludedUpValueNames[ name ] then return end
		for _, level in pairs( objectData[ self ][ "levels" ] ) do
			local idx = 0
			repeat
				idx = idx + 1
				local n, v = level.getupvalue( idx )
				if n == name then
					return v
				end
			until not n
		end
	end,
	__newindex = function( self, name, value )
		if excludedUpValueNames[ name ] then return end
		for _, level in pairs( objectData[ self ][ "levels" ] ) do
			local idx = 0
			repeat
				idx = idx + 1
				local n = level.getupvalue( idx )
				if n == name then
					level.setupvalue( idx, value )
					return
				end
			until not n
		end
	end,
	__len = function( )
		return 0
	end,
	__next = function( self, name )
		local levels = objectData[ self ][ "levels" ]
		for _, level in pairs( levels ) do
			local idx = name and 0 or -1
			repeat
				idx = idx + 1
				local n = level.getupvalue( idx )
				if name and n == name or not name then
					local value
					repeat
						idx = idx + 1
						n, value = level.getupvalue( idx )
						if n and not excludedUpValueNames[ n ] then
							return n, value
						end
					until not n
				end
			until not n
		end
	end,
	__inext = function( ) end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end,
	__ipairs = function( self )
		return function( ) end, self
	end
}

function mod.UpValues.Stacked( level )
	return mod.Proxy( { levels = mod.StackLevels( ( level or 1 ) ) }, mod.Metatables.UpValues.Stacked )
end

local excludedLocalNames = toLookup{ "(*temporary)", "(for generator)", "(for state)", "(for control)" }

mod.Metatables.Locals = {
	__index = function( self, name )
		if excludedLocalNames[ name ] then return end
		local level = objectData[ self ][ "level" ]
		local idx = 0
		repeat
			idx = idx + 1
			local n, v = level.getlocal( level, idx )
			if n == name then
				return v
			end
		until not n
	end,
	__newindex = function( self, name, value )
		if excludedLocalNames[ name ] then return end
		local level = objectData[ self ][ "level" ]
		local idx = 0
		repeat
			idx = idx + 1
			local n = level.getlocal( idx )
			if n == name then
				level.setlocal( idx, value )
				return
			end
		until not n
	end,
	__len = function( )
		return 0
	end,
	__next = function( self, name )
		local level = objectData[ self ][ "level" ]
		local idx = name and 0 or -1
		repeat
			idx = idx + 1
			local n = level.getlocal( idx )
			if name and n == name or not name then
				local value
				repeat
					idx = idx + 1
					n, value = level.getlocal( idx )
					if n and not excludedLocalNames[ n ] then
						return n, value
					end
				until not n
			end
		until not n
	end,
	__inext = function( ) end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end,
	__ipairs = function( self )
		return function( ) end, self
	end
}

mod.Locals = mod.Callable.Set( { }, function( _, level )
	return mod.Proxy( { level = mod.StackLevel( ( level or 1 ) + 1 ) }, mod.Metatables.Locals )
end )

mod.Metatables.Locals.Values = {
	__index = function( self, idx )
		local name, value = objectData[ self ][ "level" ].getlocal( idx )
		if name then
			if not excludedLocalNames[ name ] then
				return value
			end
		end
	end,
	__newindex = function( self, idx, value )
		local level = objectData[ self ][ "level" ]
		local name = level.getlocal( idx )
		if name then
			if not excludedLocalNames[ name ] then
				level.setlocal( idx, value )
			end
		end
	end,
	__len = function( self )
		local level = objectData[ self ][ "level" ]
		local idx = 1
		while level.getlocal( level, idx ) do
			idx = idx + 1
		end
		return idx - 1
	end,
	__next = function( self, idx )
		idx = idx or 0
		idx = idx + 1
		local name, val = objectData[ self ][ "level" ].getlocal( idx )
		if name then
			if not excludedLocalNames[ name ] then
				return idx, val
			end
		end
	end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end,
}
mod.Metatables.Locals.Values.__ipairs = mod.Metatables.Locals.Values.__pairs
mod.Metatables.Locals.Values.__inext = mod.Metatables.Locals.Values.__next

--[[
	Example Use:
	for i, value in pairs( mod.Locals.Values( level ) ) do
		-- stuff
	end
--]]
function mod.Locals.Values( level )
	return mod.Proxy( { level = mod.StackLevel( ( level or 1 ) + 1 ) }, mod.Metatables.Locals.Values )
end

mod.Metatables.Locals.Names = {
	__index = function( self, idx )
		local name = objectData[ self ][ "level" ].getlocal( idx )
		if name then
			if not excludedLocalNames[ name ] then
				return name
			end
		end
	end,
	__newindex = function( ) end,
	__len = mod.Metatables.Locals.Values.__len,
	__next = function( self, idx )
		if idx == nil then idx = 0 end
		idx = idx + 1
		local name = objectData[ self ][ "level" ].getlocal( idx )
		if name then
			if not excludedLocalNames[ name ] then
				return idx, name
			end
		end
	end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end,
}
mod.Metatables.Locals.Names.__ipairs = mod.Metatables.Locals.Names.__pairs
mod.Metatables.Locals.Names.__inext = mod.Metatables.Locals.Names.__next

--[[
	Example Use:
	for i, name in pairs( mod.Locals.Names( level ) ) do
		-- stuff
	end
--]]
function mod.Locals.Names( level )
	return mod.Proxy( { level = mod.StackLevel( ( level or 1 ) + 1 ) }, mod.Metatables.Locals.Names )
end

mod.Metatables.Locals.Stacked = {
	__index = function( self, name )
		if excludedLocalNames[ name ] then return end
		for _, level in pairs( objectData[ self ][ "levels" ] ) do
			local idx = 0
			repeat
				idx = idx + 1
				local n, v = level.getlocal( idx )
				if n == name then
					return v
				end
			until not n
		end
	end,
	__newindex = function( self, name, value )
		if excludedLocalNames[ name ] then return end
		for _, level in pairs( objectData[ self ][ "levels" ] ) do
			local idx = 0
			repeat
				idx = idx + 1
				local n = level.getlocal( idx )
				if n == name then
					level.setlocal( idx, value )
					return
				end
			until not n
		end
	end,
	__len = function( )
		return 0
	end,
	__next = function( self, name )
		local levels = objectData[ self ][ "levels" ]
		for _, level in pairs( levels ) do
			local idx = name and 0 or -1
			repeat
				idx = idx + 1
				local n = level.getlocal( idx )
				if name and n == name or not name then
					local value
					repeat
						idx = idx + 1
						n, value = level.getlocal( idx )
						if n and not excludedLocalNames[ n ] then
							return n, value
						end
					until not n
				end
			until not n
		end
	end,
	__inext = function( ) end,
	__pairs = function( self )
		return qrawpairs( self ), self
	end,
	__ipairs = function( self )
		return function( ) end, self
	end
}

--[[
	Access to local variables, in the current function and callers.
	The most recent definition with a given name on the call stack will
	be used.

	For example, if your function is called from CreateTraitRequirements,
	you could access its 'local screen' as mod.Locals.Stacked( ).screen
	and its 'local hasRequirement' as mod.Locals.Stacked( ).hasRequirement.
--]]
function mod.Locals.Stacked( level )
	return mod.Proxy( { levels = mod.StackLevels( level or 1 ) }, mod.Metatables.Locals.Stacked )
end

-- Entangled Data Structures

mod.Metatables.Entangled = { }

mod.Metatables.Entangled.Union = {

	__index = function( self, key )
		local value
		for t in pairs( objectData[ self ][ "Members" ] ) do
			value = t[ key ]
			if value ~= nil then
				return value
			end
		end
		return objectData[ self ][ "Reserve" ][ key ]
	end,
	__newindex = function( self, key, value )
		if value ~= nil then
			objectData[ self ][ "Keys" ][ key ] = true
		else
			objectData[ self ][ "Keys" ][ key ] = nil
		end
		local hit = false
		for t in pairs( objectData[ self ][ "Members" ] ) do
			if t[ key ] ~= nil then
				t[ key ] = value
				hit = true
			end
		end
		if hit then
			objectData[ self ][ "Reserve" ][ key ] = nil
		else
			objectData[ self ][ "Reserve" ][ key ] = value
		end
	end,
	__len = function( self )
		local m = #objectData[ self ][ "Reserve" ]
		local n
		for t in pairs( objectData[ self ][ "Members" ] ) do
			n = #t
			if n > m then
				m = n
			end
		end
		return m
	end,
	__next = function( self, key )
		key = next( objectData[ self ][ "Keys" ], key )
		return key, self[ key ]
	end,
	__inext = function( self, idx )
		idx = ( idx or 0 ) + 1
		local val = self[ idx ]
		if val == nil then return end
		return idx, val
	end,
	__pairs = function( self )
		return qrawpairs( self )
	end,
	__ipairs = function( self )
		return qrawipairs( self )
	end

}

mod.Entangled.Union = mod.Callable.Set( { }, function( _, ... )
	local keys, members = { }, toLookup{ ... }
	local union = { Reserve = { }, Keys = keys, Members = members }
	for t in pairs( members ) do
		for k in pairs( t ) do
			keys[ k ] = true
		end
	end
	return mod.Proxy( union, mod.Metatables.Entangled.Union )
end )

function mod.Entangled.Union.Add( union, ... )
	local members = objectData[ union ][ "Members" ]
	local keys = objectData[ union ][ "Keys" ]
	local reserve = objectData[ union ][ "Reserve" ]
	for t in pairs( toLookup{ ... } ) do
		members[ t ] = true
		for k in pairs( t ) do
			keys[ k ] = true
			reserve[ k ] = nil
		end
	end
end

function mod.Entangled.Union.Sub( union, ... )
	local members = objectData[ union ][ "Members" ]
	local keys = objectData[ union ][ "Keys" ]
	for t in pairs( toLookup{ ... } ) do
		members[ t ] = nil
	end
	for k in pairs( keys ) do
		if union[ k ] == nil then
			keys[ k ] = nil
		end
	end
end

mod.Metatables.Entangled.Map = {

	Data = {
		__index = function( self, key )
			return objectData[ self ][ "Map" ][ key ]
		end,
		__newindex = function( self, key, value )
			local data = objectData[ self ][ "Map" ]
			local prevValue = data[ key ]
			data[ key ] = value
			local preImage = objectData[ self ][ "PreImage" ]
			local prevKeys
			if prevValue ~= nil then
				prevKeys = preImage[ prevValue ]
			end
			local keys = preImage[ value ]
			if not keys then
				keys = { }
				preImage[ value ] = keys
			end
			if prevKeys then
				prevKeys[ key ] = nil
			end
			keys[ key ] = true
		end,
		__len = function( self )
			return #objectData[ self ][ "Map" ]
		end,
		__next = function( self, key )
			return next( objectData[ self ][ "Map" ], key )
		end,
		__inext = function( self, idx )
			return inext( objectData[ self ][ "Map" ], idx )
		end,
		__pairs = function( self )
			return qrawpairs( self )
		end,
		__ipairs = function( self )
			return qrawipairs( self )
		end
	},

	PreImage = {
		__index = function( self, value )
			return objectData[ self ][ "PreImage" ][ value ]
		end,
		__newindex = function( self, value, keys )
			objectData[ self ][ "PreImage" ][ value ] = keys
			local data = objectData[ self ][ "Map" ]
			for key in pairs( data ) do
				data[ key ] = nil
			end
			for key in ipairs( keys ) do
				data[ key ] = value
			end
		end,
		__len = function( self )
			return #objectData[ self ][ "PreImage" ]
		end,
		__next = function( self, key )
			return next( objectData[ self ][ "PreImage" ], key )
		end,
		__inext = function( self, idx )
			return inext( objectData[ self ][ "PreImage" ], idx )
		end,
		__pairs = function( self )
			return qrawpairs( self )
		end,
		__ipairs = function( self )
			return qrawipairs( self )
		end
	},

	Unique = {

		Data = {
			__index = function( self, key )
				return objectData[ self ][ "Data" ][ key ]
			end,
			__newindex = function( self, key, value )
				local data, inverse = objectData[ self ][ "Data" ], objectData[ self ][ "Inverse" ]
				if value ~= nil then
					local k = inverse[ value ]
					if k ~= key  then
						if k ~= nil then
							data[ k ] = nil
						end
						inverse[ value ] = key
					end
				end
				if key ~= nil then
					local v = data[ key ]
					if v ~= value then
						if v ~= nil then
							inverse[ v ] = nil
						end
						data[ key ] = value
					end
				end
			end,
			__len = function( self )
				return #objectData[ self ][ "Data" ]
			end,
			__next = function( self, key )
				return next( objectData[ self ][ "Data" ], key )
			end,
			__inext = function( self, idx )
				return inext( objectData[ self ][ "Data" ], idx )
			end,
			__pairs = function( self )
				return qrawpairs( self )
			end,
			__ipairs = function( self )
				return qrawipairs( self )
			end
		},

		Inverse = {
			__index = function( self, value )
				return objectData[ self ][ "Inverse" ][ value ]
			end,
			__newindex = function( self, value, key )
				local data, inverse = objectData[ self ][ "Data" ], objectData[ self ][ "Inverse" ]
				if value ~= nil then
					local k = inverse[ value ]
					if k ~= key then
						if k ~= nil then
							data[ k ] = nil
						end
						inverse[ value ] = key
					end
				end
				if key ~= nil then
					local v = data[ key ]
					if v ~= value then
						if v ~= nil then
							inverse[ v ] = nil
						end
						data[ key ] = value
					end
				end
			end,
			__len = function( self )
				return #objectData[ self ][ "Inverse" ]
			end,
			__next = function( self, value )
				return next( objectData[ self ][ "Inverse" ], value )
			end,
			__inext = function( self, idx )
				return inext( objectData[ self ][ "Inverse" ], idx )
			end,
			__pairs = function( self )
				return qrawpairs( self )
			end,
			__ipairs = function( self )
				return qrawipairs( self )
			end
		}

	}

}

mod.Entangled.Map = mod.Callable.Set( { Unique = { } }, function( )
	local data, preImage = { }, { }
	data, preImage = { Data = data, PreImage = preImage }, { Data = data, PreImage = preImage }
	data = mod.Proxy( data, mod.Metatables.Entangled.Map.Data )
	preImage = mod.Proxy( preImage, mod.Metatables.Entangled.Map.PreImage )
	return { Data = data, Index = preImage, PreImage = preImage }
end )

mod.Entangled.Map.Unique = mod.Callable.Set( { }, function( )
	local data, inverse = { }, { }
	data, inverse = { Data = data, Inverse = inverse }, { Data = data, Inverse = inverse }
	data = mod.Proxy( data, mod.Metatables.Entangled.Map.Unique.Data )
	inverse = mod.Proxy( inverse, mod.Metatables.Entangled.Map.Unique.Inverse )
	return { Data = data, Index = inverse, Inverse = inverse }
end )

-- Context Managers

local threadContexts = { }
setmetatable( threadContexts, { __mode = "kv" } )

mod.Metatables.Context = {
	__call = function( self, arg, callContext, ... )

		local thread = coroutine.running( )

		local contextInfo = {
			call = callContext,
			wrap = function( ... ) callContext( ... ) end,
			parent = threadContexts[ thread ],
			thread = thread
		}

		threadContexts[ thread ] = contextInfo

		local penv = threadEnvironments[ thread ]
		contextInfo.penv = threadEnvironments[ thread ] or _ENV_ORIGINAL

		contextInfo.context = self
		contextInfo.args = table.pack( ... )
		contextInfo.arg = arg
		contextInfo.data = { }
		contextInfo.params = table.pack( objectData[ self ][ "prepContext" ]( contextInfo ) )
		
		threadEnvironments[ thread ] = contextInfo.env
		contextInfo.response = table.pack( contextInfo.wrap( table.unpack( contextInfo.params ) ) )
		threadEnvironments[ thread ] = penv

		if objectData[ self ][ "postCall" ] then
			contextInfo.final = table.pack( objectData[ self ][ "postCall" ]( contextInfo ) )
		end

		threadContexts[ thread ] = contextInfo.parent

		if contextInfo.final then
			return table.unpack( contextInfo.final )
		end
	end
}

mod.Context = mod.Callable.Set( { }, function( _, prepContext, postCall )
	return mod.Proxy( { prepContext = prepContext, postCall = postCall }, mod.Metatables.Context )
end )

mod.Context.Data = mod.Context( function( info )
	local tbl = info.arg
	info.env = setmetatable( { }, {
		__index = function( _, key ) return tbl[ key ] or info.penv[ key ] end,
		__newindex = tbl
	} )
	info.final = { tbl }
end )

mod.Context.Meta = mod.Context( function( info )
	local tbl = mod.Node.Data.Metatable.New( info.arg )
	info.env = setmetatable( { }, {
		__index = function( _, key ) return tbl[ key ] or info.penv[ key ] end,
		__newindex = tbl
	} )
	info.final = { tbl }
end )

mod.Context.Env = mod.Context( function( info )
	local func = mod.Overriden( info.arg )
	local env = getfenv( func )
	if env == nil then return end
	if env == _ENV_REPLACED then
		env = setfenv( func, setmetatable( { }, {
			__index = _ENV_REPLACED, __newindex = _ENV_REPLACED
		} ) )
	end
	info.env = setmetatable( { }, {
		__index = function( _, key ) return rawget( env, key ) or info.penv[ key ] end,
		---@diagnostic disable-next-line: redundant-return-value
		__newindex = function( _, key, val ) return rawset( env, key, val ) end
	} )
	info.final = { env }
end )

mod.Context.Call = mod.Context(
	function( info )
		info.env = setmetatable( { }, { __index = info.penv } )
		return table.unpack( info.args )
	end,
	function( info )
		local thread = coroutine.running( )
		local penv = threadEnvironments[ thread ]
		threadEnvironments[ thread ] = info.env
		local ret = table.pack( info.arg( table.unpack( info.args ) ) )
		threadEnvironments[ thread ] = penv
		return table.unpack( ret )
	end
)

mod.Context.Wrap = mod.Callable.Set( { }, function( _, func, context, mod )
	return mod.Wrap.Bottom( func, function( base, ... ) return mod.Context.Call( base, context, ... ) end, mod )
end )


mod.Context.Wrap.Static = mod.Context(
	function( info )
		info.env = setmetatable( { }, { __index = info.penv } )
	end,
	function( info )
		return mod.Wrap( info.arg, function( base, ... ) 
			local thread = coroutine.running( )
			local penv = threadEnvironments[ thread ]
			threadEnvironments[ thread ] = info.env
			local ret = table.pack( base( ... ) )
			threadEnvironments[ thread ] = penv
			return table.unpack( ret )
		end, info.args[1] )
	end
)

-- Special Traversal Nodes

mod.Node = mod.Entangled.Map.Unique( )

function mod.Node.New( parent, key )
	local nodeType = mod.Node.Inverse[ key ]
	if nodeType then
		return key.New( parent )
	end
	local tbl = parent[ key ]
	if tbl == nil then
		tbl = { }
		parent[ key ] = tbl
	end
	return tbl
end

mod.Node.Data.Meta = {
	New = function( obj )
		local meta = getmetatable( obj )
		if meta == nil then
			meta = { }
			setmetatable( obj, meta )
		end
		return meta
	end,
	Get = function( obj )
		return getmetatable( obj )
	end,
	Set = function( obj, value )
		setmetatable( obj, value )
		return true
	end
}

mod.Node.Data.Call = {
	New = function( obj )
		return mod.Callable.Func.Get( obj ) or error( "node new rejected, new call nodes are not meaningfully mutable.", 2 )
	end,
	Get = function( obj )
		return mod.Callable.Func.Get( obj )
	end,
	Set = function( obj, value )
		mod.Callable.Set( mod.Callable.Get( obj ), value )
		return true
	end
}

mod.Node.Data.UpValues = {
	New = function( obj )
		return mod.UpValues( obj )
	end,
	Get = function( obj )
		return mod.UpValues( obj )
	end,
	Set = function( )
		error( "node set rejected, upvalues node cannot be set.", 2 )
	end
}

-- Identifier System

mod.Identifiers = mod.Entangled.Map.Unique( )
setmetatable( objectData[ mod.Identifiers.Data ][ "Inverse" ], { __mode = "k" } )
setmetatable( objectData[ mod.Identifiers.Inverse ][ "Data" ], { __mode = "v" } )

mod.Identifiers.Inverse._G = _ENV_ORIGINAL
mod.Identifiers.Inverse.mod = mod

mod.Mods = mod.Entangled.Map.Unique( )
setmetatable( objectData[ mod.Mods.Data ][ "Inverse" ], { __mode = "k" } )
setmetatable( objectData[ mod.Mods.Inverse ][ "Data" ], { __mode = "v" } )
mod.Mods.Data.mod = mod

-- Function Wrapping, Decoration, Overriding, Referral

local decorators = setmetatable( { }, { __mode = "k" } )
local overrides = setmetatable( { }, { __mode = "k" } )


local function cloneDecorNode( node, base )
	local copy = mod.Table.Copy( decorators[ node ] )
	base = base or copy.Base
	local clone = copy.Func(base)
	if decorators[ clone ] then
		error( "decorator produced duplicate reference", 2 )
	end
	decorators[ clone ] = copy
	return clone
end

local function cloneDecorHistory( parents, node, base )
	local clone = base
	while node do
		clone = cloneDecorNode( node, clone )
		node = parents[ node ]
	end
	return clone
end

local function refreshDecorHistory( parent, stop )
	local node = decorators[ parent ].Base
	if node == stop then return node end
	node = decorators[ node ] and refreshDecorHistory( node ) or node
	return cloneDecorNode( parent, node )
end

local function wrapDecorator( wrap )
	return function( base )
		return function( ... ) return wrap( base, ... ) end
	end
end

mod.Decorate = mod.Callable.Set( { }, function( _, base, func, mod )
	local out = func( base )
	if decorators[ out ] then
		error( "decorator produced duplicate reference", 2 )
	end
	decorators[ out ] = { Base = base, Func = func, Mod = mod }
	return out
end )

function mod.Decorate.Pop( obj )
	if not decorators[ obj ] then
		error( "object has no decorators", 2 )
	end
	return decorators[ obj ].Base
end

function mod.Decorate.Inject( base, func, mod )
	local out = func( base )
	if decorators[ out ] then
		error( "decorator produced duplicate reference", 2 )
	end
	local node, parent, parents = base, nil, { }
	while decorators[ node ] do
		parent = node
		node = decorators[ node ].Base
		parents[ node ] = parent
	end
	decorators[ out ] = { Base = node, Func = func, Mod = mod }
	if parent then
		return cloneDecorHistory( parents, parent, out )
	end
	return out
end

function mod.Decorate.Eject( base )
	if not decorators[ base ] then
		error( "object has no decorators", 2 )
	end
	local node, parent, parents = base, nil, { }
	while decorators[ node ] and decorators[ decorators[ node ].Base ] do
		parent = node
		node = decorators[ node ].Base
		parents[ node ] = parent
	end
	if parent then
		return cloneDecorHistory( parents, parent, decorators[ node ].Base )
	end
	return decorators[ node ].Base
end

function mod.Decorate.Refresh( obj )
	if decorators[ obj ] then
		return refreshDecorHistory( obj )
	end
	return obj
end

mod.Wrap = mod.Callable.Set( { }, function( _, base, wrap, mod )
	return mod.Decorate( base, wrapDecorator( wrap ), mod )
end )

function mod.Wrap.Bottom( base, wrap, mod )
	return mod.Decorate.Inject( base, wrapDecorator( wrap ), mod )
end

function mod.Override( base, value, mod )
	if overrides[ value ] then
		error( "cannot override with existing reference", 2 )
	end
	local node, parent, parents = base, nil, { }
	while decorators[ node ] do
		parent = node
		node = decorators[ node ].Base
		parents[ node ] = parent
	end
	if type( value ) == "function" and type( node ) == "function" then
		local env = getfenv( node )
		if getfenv( value ) == _ENV_REPLACED and env then
			setfenv( value, env )
		end
	end
	overrides[ value ] = { Base = node, Mod = mod }
	if parent then
		return cloneDecorHistory( parents, parent, value )
	end
	return value
end

function mod.Restore( base )
	local node, parent, parents = base, nil, { }
	while decorators[ node ] do
		parent = node
		node = decorators[ node ].Base
		parents[ node ] = parent
	end
	if overrides[ node ] then
		node = overrides[ node ].Base
	else
		error( "object has no overrides", 2 )
	end
	if parent then
		return cloneDecorHistory( parents, parent, node )
	end 
	return node
end

function mod.Overriden( obj )
	local node = decorators[ obj ]
	if not node then return obj end
	return mod.Overriden( node.Base )
end

function mod.Original( obj )
	local node = decorators[ obj ] or overrides[ obj ]
	if not node then return obj end
	return mod.Original( node.Base )
end

function mod.ReferFunction( obtain )
	return function( ... )
		return obtain( )( ... )
	end
end

mod.Metatables.ReferTable = {
	__index = function( self, key )
		return objectData[ self ][ "obtain" ]( )[ key ]
	end,
	__newindex = function( self, key, value )
		objectData[ self ][ "obtain" ]( )[ key ] = value
	end,
	__call = function( self, ... )
		return objectData[ self ][ "obtain" ]( )( ... )
	end,
	__len = function( self )
		return #objectData[ self ][ "obtain" ]( )
	end,
	__next = function( self, key )
		return next( objectData[ self ][ "obtain" ]( ), key )
	end,
	__inext = function( self, idx )
		return inext( objectData[ self ][ "obtain" ]( ), idx )
	end,
	__pairs = function( self )
		return pairs( objectData[ self ][ "obtain" ]( ) )
	end,
	__ipairs = function( self )
		return ipairs( objectData[ self ][ "obtain" ]( ) )
	end
}

function mod.ReferTable( obtain, ... )
	return mod.Proxy( { obtain = obtain }, mod.Metatables.ReferTable )
end

-- Management

local function setSaveIgnore(key, ignore)
	---@diagnostic disable-next-line: undefined-global
	if SaveIgnores then
		---@diagnostic disable-next-line: undefined-global
		SaveIgnores[key] = ignore
	---@diagnostic disable-next-line: undefined-global
	elseif GlobalSaveWhitelist then
		---@diagnostic disable-next-line: undefined-global
		GlobalSaveWhitelist[key] = not ignore
	end
end

--[[
	Create a namespace that can be used for the mod's functions
	and data, and ensure that it doesn't end up in save files.

	modName - the name of the mod
	parent	- the parent mod, or nil if this mod stands alone
--]]
function mod.Mod.Register( first, second, meta )
	local modName, parent
	if type( first ) == "string" then
		modName, parent = first, second
	else
		modName, parent = second, first
	end
	if not parent then
		parent = _G
		setSaveIgnore( modName, true )
	end
	local mod = parent[ modName ] or { }
	parent[ modName ] = mod
	local path = mod.Identifiers.Data[ parent ]
	if path ~= nil then
		path = path .. '.'
	else
		path = ''
	end
	path = path .. modName
	mod.Mods.Data[ path ] = mod
	mod.Identifiers.Inverse[ path ] = mod
	if meta == false then
		return mod
	end
	return setmetatable( mod, mod.Metatables.Mod )
end

-- Internal access

mod.Internal = mod.Entangled.Union( )

do
	local ups = mod.UpValues( function( )
	return _ENV_ORIGINAL, setSaveIgnore,
		objectData, newObjectData, isInt, deepLoop, repk, repv, showTableAddrs,
		decorators, overrides, refreshDecorHistory, cloneDecorHistory, cloneDecorNode,
		threadContexts, threadEnvironments, getEnv, replaceGlobalEnvironment, 
		pusherror, getname, toLookup, wrapDecorator, isNamespace,
		stackLevelFunction, stackLevelInterface, stackLevelProperty,
		passByValueTypes, callableCandidateTypes, excludedFieldNames, escapeCharacters, literalString
	end )
	mod.Entangled.Union.Add( mod.Internal, ups )
end

-- final actions

replaceGlobalEnvironment()

mod.IndexArray.Context = { }

function mod.IndexArray.Context.Env( baseTable, indexArray, context )
	return mod.Context.Env( mod.IndexArray.Get( baseTable, indexArray ), context )
end

mod.IndexArray.Wrap = mod.Callable.Set( { }, function( _, baseTable, indexArray, wrap, mod )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Wrap, wrap, mod )
end )

function mod.IndexArray.Wrap.Bottom( baseTable, indexArray, wrap, mod )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Wrap.Bottom, wrap, mod )
end

mod.IndexArray.Context.Wrap = mod.Callable.Set( { }, function( _, baseTable, indexArray, context, mod )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Context.Wrap, context, mod )
end )

function mod.IndexArray.Context.Wrap.Static( baseTable, indexArray, context, mod )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Context.Wrap.Static, context, mod )
end

mod.IndexArray.Decorate = mod.Callable.Set( { }, function( _, baseTable, indexArray, func, mod )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Decorate, func, mod )
end )

function mod.IndexArray.Decorate.Pop( baseTable, indexArray )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Decorate.Pop )
end

function mod.IndexArray.Decorate.Inject( baseTable, indexArray )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Decorate.Inject )
end

function mod.IndexArray.Decorate.Eject( baseTable, indexArray )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Decorate.Eject )
end

function mod.IndexArray.Decorate.Refresh( baseTable, indexArray )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Decorate.Refresh )
end

function mod.IndexArray.Override( baseTable, indexArray, value, mod )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Override, value, mod )
end

function mod.IndexArray.Restore( baseTable, indexArray )
	return mod.IndexArray.Map( baseTable, indexArray, mod.Restore )
end

function mod.IndexArray.Overriden( baseTable, indexArray )
	return mod.Overriden( mod.IndexArray.Get( baseTable, indexArray ) )
end

function mod.IndexArray.Original( baseTable, indexArray )
	return mod.Original( mod.IndexArray.Get( baseTable, indexArray ) )
end

function mod.IndexArray.ReferFunction( baseTable, indexArray )
	return mod.ReferFunction( function( )
		return mod.IndexArray.Get( baseTable, indexArray )
	end )
end

function mod.IndexArray.ReferTable( baseTable, indexArray )
	return mod.ReferTable( function( )
		return mod.IndexArray.Get( baseTable, indexArray )
	end )
end

---

mod.Path.Context = { }

function mod.Path.Context.Env( path, context )
	return mod.Context.Env( mod.Path.Get( path ), context )
end

mod.Path.Wrap = mod.Callable.Set( { }, function( _, path, wrap, mod )
	return mod.Path.Map( path, mod.Wrap, wrap, mod )
end )

function mod.Path.Wrap.Bottom( path, wrap, mod )
	return mod.Path.Map( path, mod.Wrap.Bottom, wrap, mod )
end

mod.Path.Context.Wrap = mod.Callable.Set( { }, function( _, path, context, mod )
	return mod.Path.Map( path, mod.Context.Wrap, context, mod )
end )

function mod.Path.Context.Wrap.Static( path, context, mod )
	return mod.Path.Map( path, mod.Context.Wrap.Static, context, mod )
end

mod.Path.Decorate = mod.Callable.Set( { }, function( _, path, func, mod )
	return mod.Path.Map( path, mod.Decorate, func, mod )
end )

function mod.Path.Decorate.Pop( path )
	return mod.Path.Map( path, mod.Decorate.Pop )
end

function mod.Path.Decorate.Refresh( path )
	return mod.Path.Map( path, mod.Decorate.Refresh )
end

function mod.Path.Override( path, value, mod )
	return mod.Path.Map( path, mod.Override, value, mod )
end

function mod.Path.Restore( path )
	return mod.Path.Map( path, mod.Restore )
end

function mod.Path.Overriden( path )
	return mod.Overriden( mod.Path.Get( path ) )
end

function mod.Path.Original( path )
	return mod.Original( mod.Path.Get( path ) )
end

function mod.Path.ReferFunction( path )
	return mod.ReferFunction( function( )
		return mod.Path.Get( path )
	end )
end

function mod.Path.ReferTable( path )
	return mod.ReferTable( function( )
		return mod.Path.Get( path )
	end )
end

--[[
    mod Main
    Components of mod that depend on loading after Main.lua
]]

-- Global Interception

--[[
	Intercept global keys which are objects to return themselves
	This way we can use other namespaces for UI etc
--]]

local callableCandidateTypes = mod.Internal.callableCandidateTypes
local setSaveIgnore = mod.Internal.setSaveIgnore

setSaveIgnore( "mod", true )

rawset( _ENV, "GLOBALS", mod.Internal._ENV_ORIGINAL )
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
		return mod.Path.Get( key )
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
		meta.__index = mod.Wrap( meta.__index, function( base, self, key )
			local value = base( self, key )
			if value ~= nil then return value end
			return routeKey( self, key )
		end, mod )
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

local objectData = mod.Internal.objectData
local passByValueTypes = mod.Internal.passByValueTypes

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
		return mod.Entangled.ModData( value )
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

mod.Metatables.Entangled.ModData = {
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

function mod.Entangled.ModData( value )
	return mod.Proxy( value, mod.Metatables.Entangled.ModData )
end

mod.Mod.Data = setmetatable( { }, {
	__call = function( _, mod )
		ModData = ModData or { }
		setSaveIgnore( "ModData", false )
		local key = mod.Mods.Inverse[ mod ]
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

relativeTable[ mod.Mod.Data ] = true

mod.Metatables.Mod = {
	__index = function( self, key )
		local val = mod.Mod[ key ]
		if relativeTable[ val ] then
			return val( self )
		elseif type( val ) == "function" then
			return mod.Wrap( val, function( base, ... )
				return base( self, ... )
			end, mod )
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
function mod.LoadOnce( triggerFunction )
	table.insert( funcsToLoad, triggerFunction )
end

--[[
	Cancel running the provided function once on the next in-game load.

	triggerFunction - the function to cancel running
--]]
function mod.CancelLoadOnce( triggerFunction )
	for i, v in ipairs( funcsToLoad ) do
		if v == triggerFunction then
			table.remove( funcsToLoad, i )
		end
	end
end

-- Internal Access

do
	local ups = mod.UpValues( function( )
		return _ENV, funcsToLoad, loadFuncs, isPath, routeKey, callableCandidateTypes, setSaveIgnore,
			objectData, passByValueTypes, modDataKey, modDataProxy, modDataPlain, relativeTable, extendGlobalEnvironment
	end )
	mod.Entangled.Union.Add( mod.Internal, ups )
end

extendGlobalEnvironment()

-- Debug Printing

local printDisplay = nil

function mod.Hades.PrintDisplay( text , delay, color )
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

function mod.Hades.PrintOverhead(text, delay, color, dest)
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

function mod.Hades.PrintStack( text, delay, color, bgcol, fontsize, font, sound )		
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
		screen.MaxStacks = mod.Hades.PrintStackCapacity
		screen.StackHeight = mod.Hades.PrintStackHeight
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

function mod.Hades.PrintStackChunks( text, linespan, ... )
	if not linespan then linespan = 90 end
	for _,s in ipairs( mod.String.Chunk( text, linespan, mod.Hades.PrintStackCapacity ) ) do
		mod.Hades.PrintStack( s, ... )
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
	local t = mod.IndexArray.Get( triggers, indexArray ) or setmetatable( { }, proxyTriggerMeta )
	local n = #t + 1
	local f = mod.Override( func, function( ... )
		return t[ n ].Call( ... )
	end )
	table.insert( t, { Args = args, Call = func } )
	mod.IndexArray.Set( triggers, indexArray, t )
	return f
end

local function proxyTrigger( name )
	mod.IndexArray.Wrap( _G, { name }, function( base, args, ... )
		local cargs = mod.Table.Copy( args )
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
mod.Hades.Triggers = triggers
mod.Identifiers.Inverse[ triggers ] = "mod.Hades.Triggers"

--]==]

return mod