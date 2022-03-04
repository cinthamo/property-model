using System.Collections.Generic;

namespace PropertiesLanguage.Test
{
	public class TestTypeContext : TypeContext
	{
        public TestTypeContext(DefinitionList definitions)
            : base(definitions, new Predefined())
        { }

        private class Predefined : IPredefined
        {
            public Dictionary<string, IType> BasicTypes
            {
                get
                {
                    return new Dictionary<string, IType>()
                    {
                        { "number", DotNetType.Int },
                        { "boolean", DotNetType.Bool },
                        { "string", DotNetType.String },
                    };
                }
            }

            public Dictionary<string, List<IType>> BasicFunctions
            {
                get
                {
                    return new Dictionary<string, List<IType>>()
                    {
                        { "==", new List<IType> { new GenericType(1), new GenericType(1), DotNetType.Bool } },
                        { "<>", new List<IType> { new GenericType(1), new GenericType(1), DotNetType.Bool } },
                        { "+", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Int } },
                        { "-", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Int } },
                        { ">=", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Bool } },
                        { "<=", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Bool } },
                        { ">", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Bool } },
                        { "<", new List<IType> { DotNetType.Int, DotNetType.Int, DotNetType.Bool } },
                        { "and", new List<IType> { DotNetType.Bool, DotNetType.Bool, DotNetType.Bool } },
                        { "or", new List<IType> { DotNetType.Bool, DotNetType.Bool, DotNetType.Bool } },
                        { "not", new List<IType> { DotNetType.Bool, DotNetType.Bool } },
                        { "GetExposedName", new List<IType> { DotNetType.String, DotNetType.String } },
                        { "GetName", new List<IType> { DotNetType.String, new ExternalType("Model"), DotNetType.String } },
                    };
                }
            }

            public Dictionary<IType, TypeObj> BasicObjects
            {
                get
                {
                    return new Dictionary<IType, TypeObj>()
                    {
                        { new ExternalType("WithIsInterface"), new TypeObj()
                            {
                                Names = new Dictionary<string, IType>()
                                {
                                    { "isInterface", DotNetType.Bool }
                                }
                            }
                        },
                        { new ExternalType("WithParent"), new TypeObj()
                            {
                                Names = new Dictionary<string, IType>()
                                {
                                    { "parent", new InternalType("AnotherObject") }
                                }
                            }
                        },
                        { new ExternalType("WithModel"), new TypeObj()
                            {
                                Names = new Dictionary<string, IType>()
                                {
                                    { "model", new ExternalType("Model") }
                                }
                            }
                        },
                        { new ExternalType("WithContext"), new TypeObj()
                            {
                                Names = new Dictionary<string, IType>()
                                {
                                    { "context", new ExternalType("Context") }
                                }
                            }
                        },
                        { new ExternalType("LocalizableImageReference"), new TypeObj() }
                    };
                }
            }

            public Dictionary<string, List<string>> BasicEnums
            {
                get
                {
                    return new Dictionary<string, List<string>>()
                    {
                        { "Country", new List<string> { "Uruguay", "Argentina", "Brasil" } },
                        { "RuntimeContext", new List<string> { "Runtime", "Design" } }
                    };
                }
            }
        }
    }
}

