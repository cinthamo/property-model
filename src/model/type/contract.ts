import PropertyDefinition from "../property/propertyDefinition";

class Contract {
    definitions: Array<PropertyDefinition>
    also: Array<Contract>

    getDefinition(name: string): PropertyDefinition {
        const definition = this.definitions.find(d => d.name == name)
        if (definition !== undefined)
            return definition

        return this.also.map(c => c.getDefinition(name)).find(d => d !== undefined)
    }
}

export default Contract