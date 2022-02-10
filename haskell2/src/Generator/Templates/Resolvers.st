using Artech.Common.Properties;

namespace Cristian.TestUC
{
	public class ResolverFactory : IResolverFactory
	{
		public IApplyResolver GetApplyResolver(string propName)
		{
$d.applyResolvers : {p|$Resolvers_Call(p)$}$
			return null;
		}

		public IContextResolver GetContextResolver()
		{
			return null;
		}

		public IApplyResolver GetCustomApplyResolver(string propName)
		{
			return null;
		}

		public IDefaultResolver GetCustomDefaultResolver(string propName)
		{
			return null;
		}

		public IReadOnlyResolver GetCustomReadOnlyResolver(string propName)
		{
			return null;
		}

		public IValidResolver GetCustomValidResolver(string propName)
		{
			return null;
		}

		public IValuesResolver GetCustomValuesResolver(string propName)
		{
			return null;
		}

		public IVisibleResolver GetCustomVisibleResolver(string propName)
		{
			return null;
		}

		public IDefaultResolver GetDefaultResolver(string propName)
		{
$d.defaultResolvers : {p|$Resolvers_Call(p)$}$
			return null;
		}

		public IAfterSetValueHandler GetOnAfterSetValueHandler(string propName)
		{
			return null;
		}

		public IReadOnlyResolver GetReadOnlyResolver(string propName)
		{
$d.readonlyResolvers : {p|$Resolvers_Call(p)$}$
			return null;
		}

		public IResourceManagerResolver GetResourceManagerResolver(string propName)
		{
			return null;
		}

		public IValidResolver GetValidResolver(string propName)
		{
$d.validResolvers : {p|$Resolvers_Call(p)$}$
			return null;
		}

		public IValuesResolver GetValuesResolver(string propName)
		{
			return null;
		}

		public IVisibleResolver GetVisibleResolver(string propName)
		{
			return null;
		}
	}

$d.defaultResolvers : {p|$Resolvers_Default(p)$}$
$d.applyResolvers : {p|$Resolvers_Apply(p)$}$
$d.readonlyResolvers : {p|$Resolvers_Readonly(p)$}$
$d.validResolvers : {p|$Resolvers_Valid(p)$}$
}
