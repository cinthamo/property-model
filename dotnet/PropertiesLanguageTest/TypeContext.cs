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

        protected struct TypeObj
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

        private static void Copy<K, T>(IDictionary<K, T> from, IDictionary<K, T> to)
        {
            foreach (KeyValuePair<K, T> p in from)
                to.Add(p.Key, p.Value);
        }

        protected TypeContext(DefinitionList definitions, IPredefined predefined)
        {
            current = new TypeObj();
            others = new Dictionary<IType, TypeObj>();

            Copy(predefined.BasicTypes, current.Names);
            Copy(predefined.BasicFunctions, current.Functions);

            foreach (KeyValuePair<string, List<string>> p in predefined.BasicEnums)
            {
                var type = new EnumType(p.Key);
                current.Names.Add(p.Key, type);

                var typeObj = new TypeObj();
                foreach (var value in p.Value)
                    typeObj.Names.Add(value, type);

                others.Add(type, typeObj);
            }

            Copy(predefined.BasicObjects, others);


            var dto = new TypeObj();

            foreach (var definition in definitions.Properties)
                dto.Names.Add(definition.Name, current.Names[definition.Type]);

            if (definitions.ExternalType != null &&
                predefined.BasicObjects.TryGetValue(new ExternalType(definitions.ExternalType), out var extObj))
            {
                Copy(extObj.Names, dto.Names);
                Copy(extObj.Functions, dto.Functions);
            }


            Copy(dto.Names, current.Names);
            Copy(dto.Functions, current.Functions);

            others.Add(new InternalType(definitions.Name), dto);
        }

        #endregion

        protected interface IPredefined
        {
            Dictionary<string, IType> BasicTypes { get; }
            Dictionary<string, List<IType>> BasicFunctions { get; }
            Dictionary<IType, TypeObj> BasicObjects { get; }
            Dictionary<string, List<string>> BasicEnums { get; }
        }
    }
}
