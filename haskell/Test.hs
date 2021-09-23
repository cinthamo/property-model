module Test where

import Model.Definition
import Model.Value

definitions :: ObjectDefinition
definitions = ObjectDefinition {
    properties = [
        Definition "one" (num 3),
        Definition "two"
            (Case
                [
                    If
                        (Func "equal" [Ref "this" "one", num 1])                    
                        (num 2)
                ]
                (Func
                    "add" [
                        Func "add" [Ref "this" "one", (num (-1))],
                        (num 4)
                    ]
                )
            )
    ],
    related = []
}

-- case 1. external properties
definitions1 :: ObjectDefinition
definitions1 = ObjectDefinition {
    properties = [
        External "Is Interface",
        Definition "Generate Object"
            (Case
                [
                    If
                        (Func "isTrue" [Ref "this" "Is Interface"])
                        false
                ]
                true
            )
    ],
    related = []
}

-- case 2. another object
definitions2 :: ObjectDefinition
definitions2 = ObjectDefinition {
    properties = [
        Definition "Auto Number" (Ref "parent" "Auto Number")
    ],
    related = ["parent"]
}

-- case 3. calculation
definitions3 :: ObjectDefinition
definitions3 = ObjectDefinition {
    properties = [
        Definition "Is Collection" false,
        Definition "Exposed Name"
            (Case
                [
                    If
                        (Ref "this" "Is Collection")
                        (str "")
                ]
                (Ref "this" "Name")
            )
    ],
    related = []
}

{-
        string dataType = properties.GetPropertyValue<string>(GxC.Properties.ATT.DataTypeString)
		string modulePath, typeName;
	*	DataTypeHelper.GetNameFromText(dataType, out modulePath, out typeName);

		string typeexp = string.IsNullOrEmpty(modulePath)? typeName : string.Format("{0}.{1}", modulePath, typeName);
		if (typeexp != null)
		{
	*		int idx = typeexp.IndexOf('(');
	*		if (idx > 0)
				typeexp = typeexp.Substring(0, idx);
			if (owner.DomainBasedOn == null)
				typeexp += "Collection";
			value = typeexp;
		}
        else
        {
            value = ""
        }
-}