module Test where

import Model.Definition
import Model.Value

definitions :: ObjectDefinition
definitions =
  ObjectDefinition
    { properties =
        [ Definition -- readonly test
            { name = "one",
              _type = "Number",
              _default = num 3,
              apply = true,
              readonly = true,
              valid = true
            },
          Definition -- default test
            { name = "two",
              _type = "Number",
              _default =
                Case
                  [(Call "==" [Ref "this" "one", num 1], num 2)]
                  ( Just
                      ( Call
                          "+"
                          [ Call "+" [Ref "this" "one", num (-1)],
                            num 4
                          ]
                      )
                  ),
              apply = true,
              readonly = false,
              valid = true
            },
          Definition -- apply test
            { name = "three",
              _type = "Boolean",
              _default = false,
              apply = false,
              readonly = false,
              valid = true
            },
          Definition -- valid test
            { name = "four",
              _type = "Number",
              _default = num 5,
              apply = true,
              readonly = false,
              valid =
                Case
                  [(Call ">" [RefValue, num 2], true)]
                  (Just false)
            }
        ],
      related = []
    }

-- case 1. external properties
definitions1 :: ObjectDefinition
definitions1 =
  ObjectDefinition
    { properties =
        [ External "Is Interface",
          Definition
            "Generate Object"
            "Boolean"
            ( Case
                [ (Call "isTrue" [Ref "this" "Is Interface"], false)
                ]
                (Just true)
            )
            true
            false
            true
        ],
      related = []
    }

-- case 2. another object
definitions2 :: ObjectDefinition
definitions2 =
  ObjectDefinition
    { properties =
        [ Definition "Auto Number" "Boolean" (Ref "parent" "Auto Number") true false true
        ],
      related = ["parent"]
    }

-- case 3. calculation
definitions3 :: ObjectDefinition
definitions3 =
  ObjectDefinition
    { properties =
        [ Definition "Is Collection" "Boolean" false true false true,
          Definition
            "Exposed Name"
            "String"
            ( Case
                [ ( Ref "this" "Is Collection",
                    Call "GetExposedName" [Ref "this" "Data Type"]
                  )
                ]
                (Just $ Ref "this" "Name")
            )
            true
            false
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
definitions4 =
  ObjectDefinition
    { properties =
        [ Definition "Image" "ImageReference" emptyValue true false true,
          Definition
            "Image Name"
            "String"
            (Call "GetName" [Ref "this" "Image", ObjRef "model"])
            true
            false
            true
        ],
      related = ["model"]
    }
