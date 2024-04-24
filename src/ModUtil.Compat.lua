
ModUtil.Mod.Register( "Compat", ModUtil )

setmetatable( ModUtil, {
	__index = ModUtil.Compat
} )

ModUtil.Compat.RegisterMod = ModUtil.Mod.Register

ModUtil.Compat.ValueString = ModUtil.ToString.Value

ModUtil.Compat.KeyString = ModUtil.ToString.Key

ModUtil.Compat.TableKeysString = ModUtil.ToString.TableKeys

ModUtil.Compat.ToShallowString = ModUtil.ToString.Shallow

ModUtil.Compat.ToDeepString = ModUtil.ToString.Deep

ModUtil.Compat.ToDeepNoNamespacesString = ModUtil.ToString.Deep.NoNamespaces

ModUtil.Compat.ToDeepNamespacesString = ModUtil.ToString.Deep.Namespaces

ModUtil.Compat.JoinStrings = ModUtil.String.Join

ModUtil.Compat.ChunkText = ModUtil.String.Chunk

ModUtil.Compat.ReplaceTable = ModUtil.Table.Replace

ModUtil.Compat.IsUnKeyed = ModUtil.Table.UnKeyed

ModUtil.Compat.PrintToFile = ModUtil.Print.ToFile

ModUtil.Compat.DebugPrint = ModUtil.Print.Debug

ModUtil.Compat.PrintTraceback = ModUtil.Print.Traceback

ModUtil.Compat.PrintNamespaces = ModUtil.Print.Namespaces

ModUtil.Compat.PrintVariables = ModUtil.Print.Variables

ModUtil.Compat.Slice = ModUtil.Array.Slice

ModUtil.Compat.NewTable = ModUtil.Node.New

ModUtil.Compat.SafeGet = ModUtil.IndexArray.Get

ModUtil.Compat.SafeSet = ModUtil.IndexArray.Set

ModUtil.Compat.MapNilTable = ModUtil.Table.NilMerge

ModUtil.Compat.MapSetTable = ModUtil.Table.Merge

ModUtil.Compat.JoinIndexArrays = ModUtil.Array.Join

ModUtil.Compat.PathToIndexArray = ModUtil.Path.IndexArray

ModUtil.Compat.PathGet = ModUtil.Path.Get

ModUtil.Compat.PathSet = ModUtil.Path.Set

ModUtil.Compat.WrapFunction = ModUtil.IndexArray.Wrap

ModUtil.Compat.RewrapFunction = ModUtil.IndexArray.Decorate.Refresh

ModUtil.Compat.UnwrapFunction = ModUtil.IndexArray.Decorate.Pop

ModUtil.Compat.WrapBaseFunction = ModUtil.Path.Wrap

ModUtil.Compat.RewrapBaseFunction = ModUtil.Path.Decorate.Refresh

ModUtil.Compat.UnwrapBaseFunction = ModUtil.Path.Decorate.Pop

ModUtil.Compat.BaseOverride = ModUtil.Path.Override

ModUtil.Compat.GetOriginalValue = ModUtil.IndexArray.Original

ModUtil.Compat.GetOriginalBaseValue = ModUtil.Path.Original

ModUtil.Compat.RawInterface = ModUtil.Raw

ModUtil.Compat.MapVars = ModUtil.Args.Map

ModUtil.Compat.StackedUpValues = ModUtil.UpValues.Stacked

ModUtil.Compat.StackedLocals = ModUtil.Locals.Stacked

ModUtil.Compat.LocalValues = ModUtil.Locals.Values

ModUtil.Compat.LocalNames = ModUtil.Locals.Names

function ModUtil.Compat.GetBaseBottomUpValues( funcPath )
	return ModUtil.UpValues( ModUtil.Path.Original( funcPath ) )
end

function ModUtil.Compat.MapTable( mapFunc, tableArg )
	return ModUtil.Table.Map( tableArg, mapFunc )
end

function ModUtil.Compat.WrapWithinFunction( baseTable, indexArray, envIndexArray, wrapFunc, mod )
	ModUtil.IndexArray.Context.Wrap( baseTable, indexArray, function( )
		ModUtil.IndexArray.Wrap( _G, envIndexArray, wrapFunc, mod )
	end )
end

function ModUtil.Compat.WrapBaseWithinFunction( funcPath, baseFuncPath, wrapFunc, mod )
	ModUtil.Path.Context.Wrap( baseFuncPath, function( )
		ModUtil.Path.Wrap( funcPath, wrapFunc, mod )
	end )
end

function ModUtil.BaseOverrideWithinFunction( funcPath, basePath, value, mod )
	ModUtil.Path.Context.Wrap( funcPath, function( )
		ModUtil.Path.Override( basePath, value, mod )
	end )
end
