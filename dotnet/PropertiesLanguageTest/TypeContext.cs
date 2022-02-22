using System;
using System.Collections.Generic;

namespace PropertiesLanguage.Test
{
	public class TypeContext : ITypeContext
	{
        #region ITypeContext

        public ITypeContext? GetContextFor(IType targetType)
        {
            if (others.TryGetValue(targetType, out var newCurrent))
                return new TypeContext(newCurrent, others);
            else
                return null;
        }

        public IType? GetTypeOfName(string name)
        {
            return current.Names.TryGetValue(name, out var type) ? type : null;
        }

        public List<IType>? GetTypesOfFuncParam(string name)
        {
            return current.Functions.TryGetValue(name, out var param) ? param : null;
        }

        #endregion

        #region Data

        private struct TypeObj
        {
            public TypeObj() { }

            public IDictionary<string, IType> Names = new Dictionary<string, IType>();
            public IDictionary<string, List<IType>> Functions = new Dictionary<string, List<IType>>();
        }

        private readonly TypeObj current;
        private readonly Dictionary<IType, TypeObj> others;

        #endregion

        #region Constructor

        private TypeContext(TypeObj c, Dictionary<IType, TypeObj> o)
        {
            current = c;
            others = o;
        }

        private static void Copy<K,T>(IDictionary<K,T> from, IDictionary<K,T> to)
        {
            foreach (KeyValuePair<K,T> p in from)
                to.Add(p.Key, p.Value);
        }

        public TypeContext(DefinitionList definitions)
        {
            current = new TypeObj();
            others = new Dictionary<IType, TypeObj>();

            Copy(BasicTypes, current.Names);
            Copy(BasicFunctions, current.Functions);

            foreach (var definition in definitions.Properties)
                current.Names.Add(definition.Name, BasicTypes[definition.Type]);

            if (definitions.ExternalType != null &&
                BasicObjects.TryGetValue(new ExternalType(definitions.ExternalType), out var extObj))
            {
                Copy(extObj.Names, current.Names);
                Copy(extObj.Functions, current.Functions);
            }

            foreach (KeyValuePair<string, List<string>> p in BasicEnums)
            {
                var type = new EnumType(p.Key);
                current.Names.Add(p.Key, type);

                var typeObj = new TypeObj();
                foreach (var value in p.Value)
                    typeObj.Names.Add(value, type);

                others.Add(type, typeObj);
            }

            Copy(BasicObjects, others);
        }

        #endregion

        #region Predefined

        private static Dictionary<string, IType> BasicTypes
        {
            get
            {
                return new Dictionary<string, IType>()
                {
                    { "number", DotNetType.Int},
                    { "boolean", DotNetType.Bool},
                    { "string", DotNetType.String },
                };
            }
        }

        private static Dictionary<string, List<IType>> BasicFunctions
        {
            get
            {
                return new Dictionary<string, List<IType>>()
                {
                    { "==", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Bool } },
                    { "+", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Int } },
                    { ">", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Bool } },
                };
            }
        }

        private static Dictionary<IType, TypeObj> BasicObjects
        {
            get
            {
                return new Dictionary<IType, TypeObj>()
                {

                };
            }
        }

        private static Dictionary<string, List<string>> BasicEnums
        {
            get
            {
                return new Dictionary<string, List<string>>()
                {

                };
            }
        }

        #endregion
    }
}
