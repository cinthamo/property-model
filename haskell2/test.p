list Test {
    definition one { // readonly test
        type = number;
        default = 3;
        readonly;
    }

    definition two { // default test
        type = number;
        default = 2 if one == 1
                | 3 if one == 2
                | (one + -1) + 4;
    }

    definition three { // apply test
        type = boolean;
        default = false;
        apply if one == 2;
    }

    definition four { // valid test
        type = number;
        default = 5;
        valid if value > 2;
    }
}

// case 1. external properties
list ExternalProperties : WithIsInterface {
    definition generateObject {
        type = boolean;
        default = not isInterface;
    }
}

// case 2. another object
list AnotherObject : WithParent {
    definition autoNumber {
        type = boolean;
        default = parent.autoNumber;
    }
}

// case 3. calculation
// Procedure GetExposedName escrito en GX
// parm(in: &DataType, out: &Value);
list Calculation {
    definition dataType {
        type = string;
        default = "a";
    }
    definition name {
        type = string;
        default = "b";
    }
    definition isCollection {
        type = boolean;
        default = false;
    }
    definition exposedName {
        type = string;
        default = GetExposedName(dataType) if isCollection
                | name;
    }
}

// case 4. object as function parameter
// procedure GetName
list ObjectAsFunctionParam : WithModel {
    definition image {
        type = string;
        default = "hola.jpg";
    }
    definition imageName {
        type = string;
        default = GetName(image, model);
    }
}

// case 5. enum
list Enum {
    definition location {
        type = Country;
        default = Country.Uruguay;
    }
    definition latin {
        type = boolean;
        default = (location) == (Country.Uruguay);
    }
}

// case 6. user control completo Upload
list Upload : WithContext {
    definition AutoUpload {
        type = boolean;
    }
    definition hideAdditionalButtons {
        type = boolean;
        apply = context == RuntimeContext.Runtime or AutoUpload;
    }
}
