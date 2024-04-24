---

ModUtil.IndexArray.Context = { }

function ModUtil.IndexArray.Context.Env( baseTable, indexArray, context )
	return ModUtil.Context.Env( ModUtil.IndexArray.Get( baseTable, indexArray ), context )
end

ModUtil.IndexArray.Wrap = ModUtil.Callable.Set( { }, function( _, baseTable, indexArray, wrap, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Wrap, wrap, mod )
end )

function ModUtil.IndexArray.Wrap.Bottom( baseTable, indexArray, wrap, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Wrap.Bottom, wrap, mod )
end

ModUtil.IndexArray.Context.Wrap = ModUtil.Callable.Set( { }, function( _, baseTable, indexArray, context, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Context.Wrap, context, mod )
end )

function ModUtil.IndexArray.Context.Wrap.Static( baseTable, indexArray, context, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Context.Wrap.Static, context, mod )
end

ModUtil.IndexArray.Decorate = ModUtil.Callable.Set( { }, function( _, baseTable, indexArray, func, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Decorate, func, mod )
end )

function ModUtil.IndexArray.Decorate.Pop( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Decorate.Pop )
end

function ModUtil.IndexArray.Decorate.Inject( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Decorate.Inject )
end

function ModUtil.IndexArray.Decorate.Eject( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Decorate.Eject )
end

function ModUtil.IndexArray.Decorate.Refresh( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Decorate.Refresh )
end

function ModUtil.IndexArray.Override( baseTable, indexArray, value, mod )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Override, value, mod )
end

function ModUtil.IndexArray.Restore( baseTable, indexArray )
	return ModUtil.IndexArray.Map( baseTable, indexArray, ModUtil.Restore )
end

function ModUtil.IndexArray.Overriden( baseTable, indexArray )
	return ModUtil.Overriden( ModUtil.IndexArray.Get( baseTable, indexArray ) )
end

function ModUtil.IndexArray.Original( baseTable, indexArray )
	return ModUtil.Original( ModUtil.IndexArray.Get( baseTable, indexArray ) )
end

function ModUtil.IndexArray.ReferFunction( baseTable, indexArray )
	return ModUtil.ReferFunction( function( )
		return ModUtil.IndexArray.Get( baseTable, indexArray )
	end )
end

function ModUtil.IndexArray.ReferTable( baseTable, indexArray )
	return ModUtil.ReferTable( function( )
		return ModUtil.IndexArray.Get( baseTable, indexArray )
	end )
end

---

ModUtil.Path.Context = { }

function ModUtil.Path.Context.Env( path, context )
	return ModUtil.Context.Env( ModUtil.Path.Get( path ), context )
end

ModUtil.Path.Wrap = ModUtil.Callable.Set( { }, function( _, path, wrap, mod )
	return ModUtil.Path.Map( path, ModUtil.Wrap, wrap, mod )
end )

function ModUtil.Path.Wrap.Bottom( path, wrap, mod )
	return ModUtil.Path.Map( path, ModUtil.Wrap.Bottom, wrap, mod )
end

ModUtil.Path.Context.Wrap = ModUtil.Callable.Set( { }, function( _, path, context, mod )
	return ModUtil.Path.Map( path, ModUtil.Context.Wrap, context, mod )
end )

function ModUtil.Path.Context.Wrap.Static( path, context, mod )
	return ModUtil.Path.Map( path, ModUtil.Context.Wrap.Static, context, mod )
end

ModUtil.Path.Decorate = ModUtil.Callable.Set( { }, function( _, path, func, mod )
	return ModUtil.Path.Map( path, ModUtil.Decorate, func, mod )
end )

function ModUtil.Path.Decorate.Pop( path )
	return ModUtil.Path.Map( path, ModUtil.Decorate.Pop )
end

function ModUtil.Path.Decorate.Refresh( path )
	return ModUtil.Path.Map( path, ModUtil.Decorate.Refresh )
end

function ModUtil.Path.Override( path, value, mod )
	return ModUtil.Path.Map( path, ModUtil.Override, value, mod )
end

function ModUtil.Path.Restore( path )
	return ModUtil.Path.Map( path, ModUtil.Restore )
end

function ModUtil.Path.Overriden( path )
	return ModUtil.Overriden( ModUtil.Path.Get( path ) )
end

function ModUtil.Path.Original( path )
	return ModUtil.Original( ModUtil.Path.Get( path ) )
end

function ModUtil.Path.ReferFunction( path )
	return ModUtil.ReferFunction( function( )
		return ModUtil.Path.Get( path )
	end )
end

function ModUtil.Path.ReferTable( path )
	return ModUtil.ReferTable( function( )
		return ModUtil.Path.Get( path )
	end )
end