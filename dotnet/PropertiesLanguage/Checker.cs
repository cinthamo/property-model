namespace PropertiesLanguage
{
    public class Checker
    {
        public Checker(TextWriter error)
        {
            Error = error;
        }

        private readonly TextWriter Error;

        public void CheckTypes(ITypeContext tc, Model model)
        {
            foreach (var definitionList in model.Definitions)
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

            ValidExpr(tc, definition.Default, type, definition.Name);
            ValidExpr(tc, definition.Apply, DotNetType.Bool, definition.Name);
            ValidExpr(tc, definition.Readonly, DotNetType.Bool, definition.Name);
            ValidExpr(tc, definition.Valid, DotNetType.Bool, definition.Name);
        }

        private void ValidExpr(ITypeContext tc, IExpression expr, IType expectedType, string propertyName)
        {
            if (expr == null)
                return;

            var found = GetType(tc, expr, expectedType, propertyName);
            if (!expectedType.Equals(found))
                Error.WriteLine($"Type mismatch in {propertyName} found {found} expected {expectedType} in expression {expr}");
        }

        private IType? GetType(ITypeContext tc, IExpression? expr, IType expectedType, string propertyName)
        {
            if (expr is StringExpression)
            {
                return DotNetType.String;
            }
            else if (expr is NumberExpression)
            {
                return DotNetType.Int;
            }
            else if (expr is BooleanExpression)
            {
                return DotNetType.Bool;
            }
            else if (expr is NullExpression)
            {
                Error.WriteLine($"Unknown type null expected {expectedType}");
                return null;
            }
            else if (expr is ValueReferenceExpression)
            {
                return expectedType;
            }
            else if (expr is NameReferenceExpression nameExpr)
            {
                return tc.GetTypeOfName(nameExpr.Name);
            }
            else if (expr is PropertyReferenceExpression propExpr)
            {
                var targetType = GetType(tc, propExpr.Target, expectedType, propertyName);
                if (targetType == null)
                    return null;

                var newTC = tc.GetContextFor(targetType);
                return newTC.GetTypeOfName(propExpr.Name);
            }
            else if (expr is CaseExpression caseExpr)
            {
                caseExpr.Conditions.ForEach(c => {
                    ValidExpr(tc, c.Condition, DotNetType.Bool, propertyName);
                    ValidExpr(tc, c.Value, expectedType, propertyName);
                });
                ValidExpr(tc, caseExpr.Otherwise, expectedType, propertyName);
                return expectedType;
            }
            else if (expr is CallExpression callExpr)
            {
                var funcTypes = tc.GetTypesOfFuncParam(callExpr.Name);
                if (funcTypes == null)
                {
                    Error.WriteLine($"Unknown function in {callExpr.Name}");
                    return null;
                }

                if (funcTypes.Count != callExpr.Parameters.Count + 1)
                    Error.WriteLine($"Incorrect parameter lenght in {propertyName}, calling {callExpr.Name} with {callExpr.Parameters.Count} parameters, declared type {funcTypes.Count} parameters");

                var callTypes = callExpr.Parameters.Select(p => GetType(tc, p, expectedType, propertyName)).ToList();
                for (int n = 0; n < callTypes.Count; n++)
                {
                    if (funcTypes[n] != callTypes[n])
                        Error.WriteLine($"Parameter type mismatch in {propertyName}, calling {callExpr.Name}, parameter {n+1}, found {callTypes[n]} expected {funcTypes[n]}");
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
