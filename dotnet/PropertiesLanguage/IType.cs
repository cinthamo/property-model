﻿using System;
using System.Diagnostics.CodeAnalysis;

namespace PropertiesLanguage
{
    public interface IType { }

    public interface ITypeContext
    {
        ITypeContext? GetContextFor(IType targetType);
        IType? GetTypeOfName(string name);
        List<IType>? GetTypesOfFuncParam(string name);
    }

    public abstract class BaseType : IType
    {
        public BaseType(string name)
        {
            Name = name;
        }

        public string Name;

        public override string ToString()
        {
            return Name;
        }

        public override bool Equals([NotNullWhen(true)] object? obj)
        {
            if (obj is BaseType o)
                return Name.Equals(o.Name);
            else
                return false;
        }

        public override int GetHashCode()
        {
            return Name.GetHashCode();
        }

        public static bool operator ==(BaseType left, BaseType right)
        {
            return left.Equals(right);
        }

        public static bool operator !=(BaseType left, BaseType right)
        {
            return !(left == right);
        }

    }

    public class DotNetType : BaseType
    {
        public DotNetType(string name, Type type)
            : base(name)
        {
            Type = type;
        }

        public Type Type;

        public static readonly IType Int = new DotNetType("number", typeof(int));
        public static readonly IType String = new DotNetType("string", typeof(string));
        public static readonly IType Bool = new DotNetType("boolean", typeof(bool));
    }

    public class ExternalType : BaseType
    {
        public ExternalType(string name)
            : base(name)
        { }
    }

    public class EnumType : BaseType
    {
        public EnumType(string name)
            : base(name)
        { }
    }
}
