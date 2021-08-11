import PropertyDefinition from "../property/propertyDefinition";

class Contract {
    definitions: Array<PropertyDefinition>
    also: Array<Contract>

    getDefinition(name: string): PropertyDefinition {
        throw new Error("Method not implemented.");
    }
}

export default Contract