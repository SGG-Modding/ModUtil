ModUtil.Callable.Context = { }

function ModUtil.Callable.Wrap( obj, wrap, mod )
	return ModUtil.Callable.Map( obj, ModUtil.Wrap, wrap, mod )
end

function ModUtil.Callable.Context.Wrap( obj, context, mod )
	return ModUtil.Callable.Map( obj, ModUtil.Context.Wrap, context, mod )
end

function ModUtil.Callable.Context.StaticWrap( obj, context, mod )
	return ModUtil.Callable.Map( obj, ModUtil.Context.StaticWrap, context, mod )
end

function ModUtil.Callable.Context.Env( obj, context )
	return ModUtil.Callable.Map( obj, ModUtil.Context.Env, context )
end

ModUtil.Callable.Decorate = ModUtil.Callable.Set( { }, function( obj, func, mod )
	return ModUtil.Callable.Map( obj, ModUtil.Decorate, func, mod )
end )

function ModUtil.Callable.Decorate.Undo( obj )
	return ModUtil.Callable.Map( obj, ModUtil.Decorate.Undo )
end

function ModUtil.Callable.Decorate.Redo( obj )
	return ModUtil.Callable.Map( obj, ModUtil.Decorate.Redo )
end

function ModUtil.Callable.Override( obj, value, mod )
	return ModUtil.Callable.Map( obj, ModUtil.Override, value, mod )
end

function ModUtil.Callable.Overriden( obj )
	return ModUtil.Callable.Map( obj, ModUtil.Overriden )
end

function ModUtil.Callable.Original( obj )
	return ModUtil.Callable.Map( obj, ModUtil.Original )
end

function ModUtil.Callable.ReferFunction( obj )
	return ModUtil.ReferFunction( ModUtil.Callable.GetFunc, obj )
end

function ModUtil.Callable.ReferTable( obj )
	return ModUtil.ReferTable( ModUtil.Callable.Get, obj )
end

---

ModUtil.IndexArray.Context = { }

function ModUtil.IndexArray.Wrap( baseTable, indexArray, wrap, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Wrap, wrap, mod )
end

function ModUtil.IndexArray.Context.Wrap( baseTable, indexArray, context, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Context.Wrap, context, mod )
end

function ModUtil.IndexArray.Context.StaticWrap( baseTable, indexArray, context, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Context.StaticWrap, context, mod )
end

function ModUtil.IndexArray.Context.Env( baseTable, indexArray, context )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Context.Wrap, context )
end


ModUtil.IndexArray.Decorate = ModUtil.Callable.Set( { }, function( baseTable, indexArray, func, mod )
	return ModUtil.Path.Map( baseTable, indexArray, ModUtil.Callable.Decorate, func, mod )
end )

function ModUtil.IndexArray.Decorate.Undo( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Decorate.Undo )
end

function ModUtil.IndexArray.Decorate.Redo( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Decorate.Redo )
end

function ModUtil.IndexArray.Override( baseTable, indexArray, value, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Override, value, mod )
end

function ModUtil.IndexArray.Overriden( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Overriden )
end

function ModUtil.IndexArray.Original( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Callable.Original )
end

function ModUtil.IndexArray.ReferFunction( baseTable, indexArray )
	return ModUtil.ReferFunction( function( ... )
		ModUtil.Callable.GetFunc( ModUtil.IndexArray.Get( ... ) )
	end, baseTable, indexArray )
end

function ModUtil.IndexArray.ReferTable( baseTable, indexArray )
	return ModUtil.ReferTable( function( ... )
		ModUtil.Callable.Get( ModUtil.IndexArray.Get( ... ) )
	end, baseTable, indexArray )
end

---

ModUtil.Path.Context = { }

function ModUtil.Path.Wrap( path, wrap, mod )
	return ModUtil.Path.Map( path, ModUtil.Callable.Wrap, wrap, mod )
end

function ModUtil.Path.Context.Wrap( path, context, mod )
	return ModUtil.Path.Map( path, ModUtil.Callable.Context.Wrap, context, mod )
end

function ModUtil.Path.Context.StaticWrap( path, context, mod )
	return ModUtil.Path.Map( path, ModUtil.Callable.Context.StaticWrap, context, mod )
end

function ModUtil.Path.Context.Env( path, context )
	return ModUtil.Path.Map( path, ModUtil.Callable.Context.Env, context )
end

ModUtil.Path.Decorate = ModUtil.Callable.Set( { }, function( path, func, mod )
	return ModUtil.Path.Map( path, ModUtil.Callable.Decorate, func, mod )
end )

function ModUtil.Path.Decorate.Undo( path )
	return ModUtil.Path.Map( path, ModUtil.Callable.Decorate.Undo )
end

function ModUtil.Path.Decorate.Redo( path )
	return ModUtil.Path.Map( path, ModUtil.Callable.Decorate.Redo )
end

function ModUtil.Path.Override( path, value, mod )
	return ModUtil.Path.Map( path, ModUtil.Callable.Override, value, mod )
end

function ModUtil.Path.Overriden( path )
	return ModUtil.Path.Map( path, ModUtil.Callable.Overriden )
end

function ModUtil.Path.Original( path )
	return ModUtil.Path.Map( path, ModUtil.Callable.Original )
end

function ModUtil.Path.ReferFunction( path )
	return ModUtil.ReferFunction( function( ... )
		ModUtil.Callable.GetFunc( ModUtil.Path.Get( ... ) )
	end, path )
end

function ModUtil.Path.ReferTable( path )
	return ModUtil.ReferTable( function( ... )
		ModUtil.Callable.Get( ModUtil.Path.Get( ... ) )
	end, path )
end