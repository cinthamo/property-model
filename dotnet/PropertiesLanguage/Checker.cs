using System.IO;
using System.Linq;

namespace Genexus.PropertiesLanguage
{
    public class Checker
    {
        public Checker(TextWriter error)
        {
            Error = error;
        }

        private readonly TextWriter Error;

        public void CheckTypes(ITypeContext tc, DefinitionList definitionList)
        {
            foreach (var definition in definitionList.Properties)
                CheckTypes(tc, definition);
        }

        private void CheckTypes(ITypeContext tc, Definition definition)
        {
            var type = tc.GetTypeOfName(definition.Type);
            if (type == null)
            {
                Error.Write($"Unknown type {definition.Type} for property {definition.Name}");
                return;
            }

            ValidExpr(tc, definition.Default, type, definition.Name, true);
            ValidExpr(tc, definition.Apply, DotNetType.Bool, definition.Name, false);
            ValidExpr(tc, definition.Readonly, DotNetType.Bool, definition.Name, false);
            ValidExpr(tc, definition.Valid, DotNetType.Bool, definition.Name, false);
        }

        private void ValidExpr(ITypeContext tc, IExpression expr, IType expectedType, string propertyName, bool optional)
        {
            if (expr == null || optional && expr is NullExpression)
                return;

            var found = GetTypeCheckNull(tc, expr, propertyName);
            if (found != null && !expectedType.Equals(found))
                Error.WriteLine($"Type mismatch in {propertyName} found {found} expected {expectedType} in expression {expr}.");
        }

        private IType? GetTypeCheckNull(ITypeContext tc, IExpression? expr, string propertyName)
        {
            var found = GetType(tc, expr, propertyName);
            if (found == null)
                Error.WriteLine($"Type unknown for expression {expr}.");
            return found;
        }


        private IType? GetType(ITypeContext tc, IExpression? expr, string propertyName)
        {
            if (expr is StringExpression)
            {
                return DotNetType.String;
            }
            else if (expr is NumericExpression)
            {
                return DotNetType.Int;
            }
            else if (expr is BooleanExpression)
            {
                return DotNetType.Bool;
            }
            else if (expr is NullExpression)
            {
                return null;
            }
            else if (expr is ValueReferenceExpression)
            {
                return tc.GetTypeOfName(propertyName);
            }
            else if (expr is NameReferenceExpression nameExpr)
            {
                return tc.GetTypeOfName(nameExpr.Name);
            }
            else if (expr is PropertyReferenceExpression propExpr)
            {
                var targetType = GetTypeCheckNull(tc, propExpr.Target, propertyName);
                if (targetType == null)
                    return null;

                var newTC = tc.GetContextFor(targetType);
                if (newTC == null)
                    return null;

                return newTC.GetTypeOfName(propExpr.Name);
            }
            else if (expr is CaseExpression caseExpr)
            {
                var type = GetTypeCheckNull(tc, caseExpr.Otherwise, propertyName);
                if (type == null)
                    return null;

                caseExpr.Conditions.ForEach(c => {
                    ValidExpr(tc, c.Condition, DotNetType.Bool, propertyName, false);
                    var found = GetTypeCheckNull(tc, c.Value, propertyName);
                    if (found != null && !type.Equals(found))
                        Error.WriteLine($"Type mismatch in {propertyName} found {found} expected {type} in expression {expr}");
                });

                return type;
            }
            else if (expr is CallExpression callExpr)
            {
                var funcTypes = tc.GetTypesOfFuncParam(callExpr.Name);
                if (funcTypes == null)
                {
                    Error.WriteLine($"Unknown function in {callExpr.Name} ({callExpr.Position})");
                    return null;
                }

                if (funcTypes.Count != callExpr.Parameters.Count + 1)
                    Error.WriteLine($"Incorrect parameter lenght in {propertyName}, calling {callExpr.Name} with {callExpr.Parameters.Count} parameters, declared type {funcTypes.Count} parameters");

                var callTypes = callExpr.Parameters.Select(p => GetTypeCheckNull(tc, p, propertyName)).ToList();
                for (int n = 0; n < callTypes.Count; n++)
                {
                    if (callTypes[n] != null)
                        continue;

                    if (funcTypes[n] is GenericType gn)
                    {
                        string id = gn.Name;
                        for (int m = n; m < funcTypes.Count; m++)
                        {
                            if (funcTypes[m] is GenericType gm && gm.Name == id)
                                funcTypes[m] = callTypes[n];
                        }
                    }

                    if (!funcTypes[n].Equals(callTypes[n]))
                        Error.WriteLine($"Parameter type mismatch in {propertyName}, calling {callExpr.Name}, parameter {n+1}, found {callTypes[n]} expected {funcTypes[n]} ({callExpr.Parameters[n].Position}).");
                }

                return funcTypes.Last();
            }
            else
            {
                Error.WriteLine("Unknown expression");
                return null;
            }
        }
    }
}
