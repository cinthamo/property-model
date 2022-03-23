type Test {
    /// readonly test
    one: numeric {
        default = 3
        readonly
    }

    /// default test
    two: numeric {
        default = 2 if one == 1
                | 3 if one == 2
                | (one + -1) + 4
    }

    /// apply test
    three: boolean {
        default = false
        apply if one == 2
    }

    /// valid test
    four: numeric {
        default = 5
        valid if value > 2
    }

    /// precedence test
    five: numeric {
        readonly if (four == 4) or (three and (two == (2 + 1)))
    }
}

// case 1. external properties
type extends WithIsInterface {
    generateObject: boolean {
        default = not isInterface
    }
}

// case 2. another object
type extends WithParent {
    autoNumber: boolean {
        default = parent.autoNumber
    }
}

// case 3. calculation
// Procedure GetExposedName escrito en GX
// parm(in: &DataType, out: &Value);
type Calculation {
    dataType: string {
        default = "a"
    }
    name: string {
        default = "b"
    }
    isCollection: boolean
    exposedName: string {
        default = GetExposedName(dataType) if isCollection
                | name
    }
}

// case 4. object as function parameter
// procedure GetName
type extends WithModel {
    image: LocalizableImageReference
    imageName: string {
        default = GetName(image, model)
    }
}

// case 5. enum
type Enum {
    location: Country {
        default = Country.Uruguay
    }
    latin: boolean {
        default = location == (Country.Uruguay)
    }
}

// case 6. user control completo Upload
type extends WithContext {
    AutoUpload: boolean
    HideAdditionalButtons: boolean {
        apply = (context == (RuntimeContext.Runtime)) or AutoUpload
    }
}
