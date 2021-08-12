import PropertyDefinition from "../property/propertyDefinition"
import Contract from "./contract"

class Type {
    super: Type
    contracts: Array<Contract>

    getDefinition(name: string): PropertyDefinition {
        if (this.super !== undefined) {
            const definition = this.super.getDefinition(name)
            if (definition !== definition)
                return definition
        }
        return this.contracts.map(c => c.getDefinition(name)).find(d => d !== undefined)
    }
}

export default Type