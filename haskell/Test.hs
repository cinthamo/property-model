module Test where

import Model.Definition
import Model.Value

definitions :: ObjectDefinition
definitions = ObjectDefinition {
    properties = [
        Definition "one" (num 3) true,
        Definition "two"
            (Case
                [
                    (Call "equal" [Ref "this" "one", num 1], num 2)
                ]
                (Just (Call
                    "add" [
                        Call "add" [Ref "this" "one", (num (-1))],
                        (num 4)
                    ]
                ))
            )
            true,
        Definition "three" false false
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
                    (Call "isTrue" [Ref "this" "Is Interface"], false)
                ]
                (Just true)
            )
            true
    ],
    related = []
}

-- case 2. another object
definitions2 :: ObjectDefinition
definitions2 = ObjectDefinition {
    properties = [
        Definition "Auto Number" (Ref "parent" "Auto Number") true
    ],
    related = ["parent"]
}

-- case 3. calculation
definitions3 :: ObjectDefinition
definitions3 = ObjectDefinition {
    properties = [
        Definition "Is Collection" false true,
        Definition "Exposed Name"
            (Case
                [
                    (Ref "this" "Is Collection",
                    Call "GetExposedName" [Ref "this" "Data Type"])
                ]
                (Just $ Ref "this" "Name")
            )
            true
    ],
    related = []
}

{-
Procedure GetExposedName
parm(in: &DataType, out: &Value);

// Write this as a GX procedure
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

-- case 4. object as function parameter
definitions4 :: ObjectDefinition
definitions4 = ObjectDefinition {
    properties = [
        Definition "Image" emptyValue true,
        Definition "Image Name"
            (Call "GetName" [Ref "this" "Image", ObjRef "model"])
            true
    ],
    related = ["model"]
}
